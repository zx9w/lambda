module Lambda where


type Var = String

data Expr = Var
          | Lambda Var Expr
          | Apply Expr Expr
