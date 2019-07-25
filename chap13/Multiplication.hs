module Multiplication where

multiplyBy :: Integral a => a -> a -> a
multiplyBy x y = 
    if (y > 0 && x > 0) || (y < 0 && x < 0)
    then go (abs x) (abs y)
    else - (go (abs x) (abs y))
    where go x y
           | x == 0 || y == 0 = 0
           | otherwise = x + go x (y - 1)