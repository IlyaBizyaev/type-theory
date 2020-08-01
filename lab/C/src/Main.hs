module Main where

import           Data.List       (find, intercalate, partition, union)
import qualified Data.Map.Strict as Map
import           Lexer
import           Parser

data Type = TImpl Type Type
    | TVar String
    deriving (Eq, Ord)

instance Show Type where
  show (TImpl x y) = "(" ++ show x ++ " -> " ++ show y ++ ")"
  show (TVar name) = name

data TypeEqSystem = TypeEqSystem [(Type, Type)]

instance Show TypeEqSystem where
  show (TypeEqSystem x) =
    intercalate "\n" (map (\(l, r) -> show l ++ " = " ++ show r) x)

data TypedExpression = TypedExpression Expression Type

instance Show TypedExpression where
  show (TypedExpression e t) = show e ++ " : " ++ show t

data Rule = Rule Int
instance Show Rule where
  show (Rule x) = "[rule #" ++ show x ++ "]"

data Proof = SuccessfulProof
    { proofContext    :: [TypedExpression]
    , proofExpression :: TypedExpression
    , proofRule       :: Rule
    , proofDepends    :: [Proof]
    }
    | FailedProof

showGoodProof :: Int -> Proof -> String
showGoodProof depth p =
  let
    sep       = "*   "
    turnstile = "|- "
    printSep n = concat $ replicate n sep
    strContext = intercalate ", " (map show (proofContext p))
    curLine =
      (printSep depth)
        ++ strContext
        ++ (if null (proofContext p) then "" else " ")
        ++ turnstile
        ++ (show $ proofExpression p)
        ++ " "
        ++ show (proofRule p)
    strProof =
      intercalate "\n" (map (showGoodProof (depth + 1)) (proofDepends p))
  in
    curLine ++ (if null (proofDepends p) then "" else ("\n" ++ strProof))

instance Show Proof where
  show (FailedProof) = "Expression has no type"
  show x             = showGoodProof 0 x

-- returning: (equat system, exprType, free variables, lastType)
buildTESandFV
  :: Expression
  -> Int
  -> Map.Map Expression Type
  -> Map.Map Expression Type
  -> (TypeEqSystem, Type, Map.Map Expression Type, Int)
buildTESandFV expr lastType abstrVMap fvMap =
  let makeNewType curLast = TVar ("t" ++ show (curLast + 1))
  in  case expr of
        (Var name) | Map.member expr abstrVMap ->
          (TypeEqSystem [], abstrVMap Map.! expr, fvMap, lastType)
        (Var name) | Map.member expr fvMap ->
          (TypeEqSystem [], fvMap Map.! expr, fvMap, lastType)
        (Var name) ->
          let newType = makeNewType lastType
          in  ( TypeEqSystem []
              , newType
              , Map.insert expr newType fvMap
              , lastType + 1
              )
        (Appl p q) ->
          let (TypeEqSystem eP, typeP, fvMapP, lastTypeP) =
                  buildTESandFV p lastType abstrVMap fvMap
              (TypeEqSystem eQ, typeQ, fvMapQ, lastTypeQ) =
                  buildTESandFV q lastTypeP abstrVMap fvMapP
              newType = makeNewType lastTypeQ
              ePQ     = TypeEqSystem ((typeP, TImpl typeQ newType) : (eP ++ eQ))
          in  (ePQ, newType, fvMapQ, lastTypeQ + 1)
        (Abstr x p) ->
          let newType      = makeNewType lastType
              newAbstrVMap = Map.insert x newType abstrVMap
              (eP, typeP, fvMapP, lastTypeP) =
                  buildTESandFV p (lastType + 1) newAbstrVMap fvMap
          in  (eP, TImpl newType typeP, fvMapP, lastTypeP)

-- Take the built system and attempt to solve it, applying unification operations
-- until the system either becomes not solvable or stops changing (i.e. gets solved)
solveTypeEqSystem :: TypeEqSystem -> Maybe TypeEqSystem
solveTypeEqSystem (TypeEqSystem tes) =
  let
    pairSwapSidesIfNecessary p = case p of
      ((TImpl x y), TVar z) -> (TVar z, (TImpl x y))
      _                     -> p
    swapSidesIfNecessary l = map pairSwapSidesIfNecessary l
    pairIsNotTrivial (a, b) = a /= b
    removeTrivial l = filter pairIsNotTrivial l
    pairIsTermReducible p = case p of
      ((TImpl a b), (TImpl c d)) -> True
      _                          -> False
    reduceTerms l =
      let (reduc, nonreduc) = partition pairIsTermReducible l
          pairReduce p = case p of
            ((TImpl a b), (TImpl c d)) -> [(a, c), (b, d)]
            _                          -> error "Non-reducible unexpected!"
      in  concat (map pairReduce reduc) ++ nonreduc
    pairIsASubstitution p = case p of
      (TVar a, _) -> True
      _           -> False
    performSubstitution used l =
      let substExprByOne expr (a, b) = case expr of
            (TVar name) -> if expr == a then b else expr
            (TImpl x y) ->
              (TImpl (substExprByOne x (a, b)) (substExprByOne y (a, b)))
          substPairByOne (l, r) x = (substExprByOne l x, substExprByOne r x)
          substSysByOne [] _ = []
          substSysByOne ((l1, l2) : ls) x =
              ((substPairByOne (l1, l2) x) : (substSysByOne ls x))
      in  case (find pairIsASubstitution l) of
            Just firstSub ->
              let remaining = filter (/= firstSub) l
              in  performSubstitution
                    ((substSysByOne used firstSub) ++ [firstSub])
                    (substSysByOne remaining firstSub)
            Nothing -> used ++ l
    exprContainsVar expr v = case expr of
      (TVar name) | expr == v                                      -> True
      (TImpl x y) | (exprContainsVar x v) || (exprContainsVar y v) -> True
      _                                                            -> False
    pairIsNotSolvable p = case p of
      (TVar name, TImpl x y) | exprContainsVar (TImpl x y) (TVar name) -> True
      _ -> False
    systemIsNotSolvable [] = False
    systemIsNotSolvable (l : ls) =
      (pairIsNotSolvable l) || (systemIsNotSolvable ls)
    executeOneSolutionIteration =
      (performSubstitution [])
        . swapSidesIfNecessary
        . reduceTerms
        . removeTrivial
  in
    if systemIsNotSolvable tes
      then Nothing
      else
        let afterIteration = executeOneSolutionIteration tes
            result         = if afterIteration /= tes
              then solveTypeEqSystem (TypeEqSystem afterIteration)
              else Just (TypeEqSystem tes)
        in  result


-- Builds proof structure when all of the data is available
-- context, typedExpression, rule, depends
-- returning: (proof, exprType, free variables, lastType)
makeProof
  :: Expression
  -> Int
  -> Map.Map Expression Type
  -> Map.Map Expression Type
  -> Map.Map Type Type
  -> [TypedExpression]
  -> (Proof, Type, Map.Map Expression Type, Int)
makeProof expr lastType abstrVMap fvMap substMap parentContext =
  let
    makeNewType curLast = TVar ("t" ++ show (curLast + 1))
    applySubst t = Map.findWithDefault t t substMap
  in
    case expr of
      (Var name) | Map.member expr abstrVMap ->
        let thisType = applySubst (abstrVMap Map.! expr)
        in  ( SuccessfulProof parentContext
                              (TypedExpression expr thisType)
                              (Rule 1)
                              []
            , thisType
            , fvMap
            , lastType
            )
      (Var name) | Map.member expr fvMap ->
        let thisType = applySubst (fvMap Map.! expr)
        in  ( SuccessfulProof parentContext
                              (TypedExpression expr thisType)
                              (Rule 1)
                              []
            , thisType
            , fvMap
            , lastType
            )
      (Var name) ->
        let newType  = makeNewType lastType
            thisType = applySubst newType
        in  ( SuccessfulProof parentContext
                              (TypedExpression expr thisType)
                              (Rule 1)
                              []
            , thisType
            , Map.insert expr newType fvMap
            , lastType + 1
            )
      (Appl p q) ->
        let (proofP, typeP, fvMapP, lastTypeP) =
                makeProof p lastType abstrVMap fvMap substMap parentContext
            (proofQ, typeQ, fvMapQ, lastTypeQ) =
                makeProof q lastTypeP abstrVMap fvMapP substMap parentContext
            newType  = makeNewType lastTypeQ
            thisType = applySubst newType
        in  ( SuccessfulProof parentContext
                              (TypedExpression expr thisType)
                              (Rule 2)
                              [proofP, proofQ]
            , thisType
            , fvMapQ
            , lastTypeQ + 1
            )
      (Abstr x p) ->
        let
          newType                            = makeNewType lastType
          thisType                           = applySubst newType
          newAbstrVMap                       = Map.insert x newType abstrVMap
          newParentContext = (TypedExpression x thisType) : parentContext
          (proofP, typeP, fvMapP, lastTypeP) = makeProof p
                                                         (lastType + 1)
                                                         newAbstrVMap
                                                         fvMap
                                                         substMap
                                                         newParentContext
        in
          ( SuccessfulProof parentContext
                            (TypedExpression expr (TImpl thisType typeP))
                            (Rule 3)
                            [proofP]
          , TImpl thisType typeP
          , fvMapP
          , lastTypeP
          )


deduceTypes :: Expression -> Proof
deduceTypes expr =
  let (tes, _, fvMap, _) = buildTESandFV expr 0 Map.empty Map.empty
      maybeSubstitution  = solveTypeEqSystem tes
  in  case maybeSubstitution of
        Nothing -> FailedProof
        Just (TypeEqSystem substitution) ->
          let
            substMap = Map.fromList substitution
            applySubst t = Map.findWithDefault t t substMap
            fvContext = map (\(a, b) -> TypedExpression a (applySubst b))
                            (Map.toList fvMap)
            (resultingProof, _, _, _) =
              makeProof expr 0 Map.empty Map.empty substMap fvContext
          in
            resultingProof

main = do
  s <- getContents
  let expr = (parser . alexScanTokens) s
  putStrLn $ show (deduceTypes expr)
