module Parser where

import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Data.Char(isLetter, isNumber)
import Lambda

init_parser :: IO String
init_parser = do
  args <- getArgs
  if' (length args == 1) (readFile $ head args) (error "specify source file please")

if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

expression :: ReadP Expr
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

lambda :: ReadP Expr
lambda = do
  char '\\' -- backslash stands for lambda"  v <- variable
  e0 <- expression
  char '.'
  e1 <- expression
  return $ Apply e0 e1
