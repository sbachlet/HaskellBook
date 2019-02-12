module Scans where
fibs = 1 : scanl (+) 1 fibs
fibs20 = take 20 $ fibs
fibsLT100 = takeWhile (<100) fibs
fibsN x = fibs !! x

factorial = scanl (*) 1 [1..]
takeFact x = take x factorial
