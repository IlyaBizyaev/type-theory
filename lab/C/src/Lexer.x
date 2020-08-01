{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-z]

tokens :-
  $white                                    ;
  "\"                                       { \_ -> TkLambda }
  "."                                       { \_ -> TkSpan }
  "("                                       { \_ -> TkPLeft }
  ")"                                       { \_ -> TkPRight }
  $alpha [$alpha $digit \']*                { \s -> TkVar s}

{
data Token = TkLambda | TkSpan | TkPLeft | TkPRight | TkVar String deriving (Eq, Show)
}
