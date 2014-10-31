module Calc where

import Parser

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr = maybeEval . parseExp Lit Add Mul

maybeEval :: Maybe ExprT -> Maybe Integer
maybeEval (Just t) = Just (eval t)
maybeEval Nothing = Nothing

class Expr a where
  lit :: Integer -> a
  mul :: a -> a -> a
  add :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  mul = Mul
  add = Add

instance Expr Integer where
  lit x = x
  mul = (*)
  add = (+)
  
instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  mul = (&&)
  add = (||)