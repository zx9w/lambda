module Lambda where

data Nat = Z | S Nat deriving (Eq)

data Expr = Var Nat
          | Lambda Nat Expr
          | Apply Expr Expr


beta :: Expr -> Expr
beta expr = case expr of
  (Apply (Lambda x e) v) -> case e of
                                    (Var x') -> if x==x' then v else (Var x')
                                    _ -> e
