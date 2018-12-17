module SquareCube where
mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [x^3 | x <- [1..5]]

-- 1.
one = [(x, y) | x <- mySqr, y <- myCube]

-- 2.
two = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

-- 3.
three = length two