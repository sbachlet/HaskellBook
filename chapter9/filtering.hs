module Filtering where
list = [1..30]
one = filter (\x -> rem x 3 == 0)
two x = length . one $ x
three y = filter (\x -> not $ elem x ["the", "a", "an"]) . words $ y