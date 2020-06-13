module Parser where

import System.Environment (getArgs)
import Text.ParserCombinators.ReadP
import Data.Char(isLetter)
import Lambda


init_parser :: IO () --String
init_parser = do
  args <- getArgs
  putStrLn $ concat getArgs

  -- if' (length args == 1) (readFile $ head args) (error "specify source file please")

if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

-- data Expr = Var Char
--           | Lambda Char Expr
--           | Apply Expr Expr

parse :: String -> Maybe Expr
parse s = case (readP_to_S expr s) of
  [(e,_)] -> Just e
  _       -> Nothing

expr :: ReadP Expr
expr = var +++ lambda +++ apply

var :: ReadP Expr
var = do
  c <- satisfy isLetter
  return $ Var c

lambda :: ReadP Expr
lambda = do
  v <- satisfy isLetter
  e <- expr
  return $ Lambda v e

apply :: ReadP Expr
apply = do
  char '\\'
  e0 <- expr
  char '.'
  e1 <- expr
  return $ Apply e0 e1
