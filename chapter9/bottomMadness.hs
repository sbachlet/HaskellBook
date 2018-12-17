module BottomMadness where
-- 1. Bottom
[x^y | x <- [1..5], y <- [2, undefined]]
-- 2. Value
take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]
-- 3. Bottom
sum [1, undefined, 3]
-- 4. Value
length [1, 2, undefined]
-- 5. Bottom
length $ [1, 2, 3] ++ undefined
-- 6. Value
take 1 $ filter even [1, 2, 3, undefined]
-- 7. Bottom
take 1 $ filter even [1, 3, undefined]
-- 8. Value
take 1 $ filter odd [1, 3, undefined]
-- 9. Value
take 2 $ filter odd [1, 3, undefined]
-- 10. Bottom
take 3 $ filter odd [1, 3, undefined]