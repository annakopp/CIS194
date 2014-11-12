import Data.List

fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) (drop 1 fibs2) fibs2
