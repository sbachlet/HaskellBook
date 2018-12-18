module MoreBottoms where
import Data.Bool
-- 1. Bottom
-- 2. [2]
-- 3. Bottom
-- 4. This will return a boolean value on whether the letter in the string is vowel
-- 5a. [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
-- 5b. [1, 10, 20]
-- 5c. [15, 15, 15]
-- 6.
-- map (\x -> if x == 3 then (-x) else (x)) [1..10]
test = map (\x -> bool (x::Integer) (-(x::Integer)) (x::Integer) == 3) [1..10]