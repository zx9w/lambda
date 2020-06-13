module Parser where

import System.Environment (getArgs)

init_parser :: IO String
init_parser = do
  args <- getArgs
  if' (length args == 1) (readFile $ head args) (error "specify source file please")

if' :: Bool -> a -> a -> a
if' b x y = if b then x else y
