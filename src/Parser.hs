module Parser where

import System.Environment (getArgs)
import ReadP
import Data.Char(isLetter, isNumber)

init_parser :: IO String
init_parser = do
  args <- getArgs
  if' (length args == 1) (readFile $ head args) (error "specify source file please")

if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

var :: ReadP String
var = many alphaNum
  where
    alphaNum = satisfy isLetter +++ satisfy isNumber
