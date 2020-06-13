module Lambda where

import Data.Number.Natural

data Nat = Z | S Nat deriving (Eq)

getNat :: Nat -> Natural
getNat Z = 0
getNat (S n) = 1 + (getNat n)

instance Show Nat where
  show n = show (getNat n)

data Expr = Var Nat
          | Lambda Nat Expr
          | Apply Expr Expr
<<<<<<< HEAD
          deriving Show
=======


fix :: Nat -> Expr -> Expr -> Expr
fix n expr input = case expr of
  (Var x) -> if x==n then input else Var x
  (Lambda m e) -> (Lambda m (fix n e input)) -- can assume m!=n because this is a '\x.\y.' setup
  (Apply e1 e2) -> (Apply (fix n e1 input) (fix n e2 input))

beta :: Expr -> Expr
beta expr = case expr of
  (Apply (Lambda x e) v) -> fix x e v
>>>>>>> bc794d03f8acaf53dc9529a55f8d6c4811537e26
