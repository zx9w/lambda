module Parser where

import Text.ParserCombinators.ReadP
import Data.Char(isLetter, isNumber)
import Lambda

expression :: ReadP Expression
expression = var +++ lambda +++ apply

var :: ReadP String -- TODO change to ReadP Expr
var = many alphaNum
  where
    alphaNum = satisfy isLetter +++ satisfy isNumber

apply :: ReadP Expr
apply = do
  char '('
  e <- expression
  char ')'
  return e
