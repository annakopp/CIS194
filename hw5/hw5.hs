module Calc where

import Parser

newtype MinMax = MinMax Integer deriving (Eq, Show)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

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
  
instance Expr MinMax where
  lit = MinMax
  mul (MinMax x) (MinMax y) = lit (min x y)
  add (MinMax x) (MinMax y) = lit (max x y)
  
instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  mul (Mod7 x) (Mod7 y) = lit (x+y)
  add (Mod7 x) (Mod7 y) = lit (x*y)
  
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7
