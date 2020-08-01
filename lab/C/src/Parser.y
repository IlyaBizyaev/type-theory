{
module Parser where

import Lexer
}

%name parser
%tokentype { Token }
%error { parseError }

%token
  var         { TkVar $$ }
  '\\'        { TkLambda }
  '.'         { TkSpan }
  '('         { TkPLeft }
  ')'         { TkPRight }

%%

Expression:
  Application '\\' var '.' Expression     { Appl $1 (Abstr (Var $3) $5) }
  | '\\' var '.' Expression               { Abstr (Var $2) $4 }
  | Application                           { $1 }

Application:
  Application Atom              { Appl $1 $2 }
  | Atom                        { $1 }

Atom:
  '(' Expression ')'            { $2 }
  | var                         { Var $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Expression = Appl Expression Expression
                | Abstr Expression Expression
                | Var String deriving (Eq, Ord)

instance Show Expression where
  show (Appl x y)  = "(" ++ show x ++ " " ++ show y ++ ")"
  show (Abstr x y) = "(\\" ++ show x ++ "." ++ show y ++ ")"
  show (Var name)  = name
}
