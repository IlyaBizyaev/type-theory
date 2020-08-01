module Main where

import Lexer
import Parser

main = do
  s <- getContents
  putStrLn $ show $ (parser . alexScanTokens) s
