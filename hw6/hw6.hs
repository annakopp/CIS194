{-# LANGUAGE FlexibleInstances #-}

import Data.List

fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) (drop 1 fibs2) fibs2

data Stream a = Cons (a) (Stream a)

instance Show a => Show (Stream a) where
show = take 20 . streamToList


streamToList :: Stream a -> [a]
streamToList (Cons x xs) = [x] ++ streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons (x) (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f el = Cons (el) (streamFromSeed f (f el) ) 

nats :: Stream Integer
nats = streamFromSeed (\x -> x+1) 0

interleavesStreams :: Stream a -> Stream a -> Stream a
interleavesStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleavesStreams xs ys))

-- Define the stream
-- ruler :: Stream Integer
-- which corresponds to the ruler function
-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2, 0, 1. 0, 3, 0, 1, 0, 
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.
-- interleavesStreams (streamRepeat 0) (interleavesStreams (streamRepeat 1) ((interleavesStreams (streamRepeat 2)) (interleavesStreams (streamRepeat 3) (streamRepeat 4))))
--
-- ruler :: Stream Integer
-- ruler = interleavesStreams (streamRepeat 0) (interleavesStreams (streamRepeat 1) (streamRepeat 2))


-- #6

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger int =  Cons int (streamRepeat 0)
  negate = streamMap (\x -> x * (-1))
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
  (*) (Cons x xs) (Cons y ys) = Cons (x*y) ((streamMap (\el -> el * x) ys) + (xs * Cons y ys))

instance Fractional (Stream Integer) where
  (/) (Cons x xs) (Cons y ys) = Cons (x `div` y) (streamMap (\el -> el `div` y) (xs - ((Cons x xs)/(Cons y ys) * (ys))))
  
fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)