module Exercises where
-- 1
-- a
stops = "pbtdkg"
vowels = "aeiou"
wordTripleFactory :: String -> String -> [(Char, Char, Char)]
wordTripleFactory s _ = map (\x -> (x, x, x)) s
-- b
-- c