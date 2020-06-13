module Lambda where

data Expr = Var Char
          | Lambda Char Expr
          | Apply Expr Expr
