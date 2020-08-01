{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-
  $white                                    ;
  "\"                                       { \_ -> TLambda }
  "."                                       { \_ -> TSpan }
  "("                                       { \_ -> TPLeft }
  ")"                                       { \_ -> TPRight }
  $alpha [$alpha $digit \']*                { \s -> TVar s}

{
data Token = TLambda | TSpan | TPLeft | TPRight | TVar String deriving (Eq, Show)
}
