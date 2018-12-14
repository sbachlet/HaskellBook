module EightSix where
-- Chapter Exercises
-- Review of Types ----------------------
-- 1. D
-- 2. B
-- 3. D
-- 4. B
-- Reviewing Currying -------------------
cattyConny :: String -> String -> String
cattyConny x y  = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"
-- 1. "woops mrow woohoo!"
-- 2. "1 mrow haha"
-- 3. "woops mrow 2 mrow haha"
-- 4. appedCatty (frappe "blue") = "woops mrow blue mrow haha"
-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) = "pink mrow haha mrow green mrow woops mrow blue"
-- 6. cattyConny (flippy "Pugs" "are") "awesome" = "are mrow Pugs mrow awesome"

-- Recursion --------------------------
-- 1. divideBy 15 2 => go 15 2 0
-- -> go 13 2 1
-- -> go 11 2 2
-- -> go 9 2 3
-- -> go 7 2 4
-- -> go 5 2 5
-- -> go 3 2 6
-- -> go 1 2 7
-- -> (7, 1)
-- 2.
sumBelow :: (Eq a, Num a) => a -> a
sumBelow x
  | abs x == x = go 1
  | otherwise  = goNeg 1
  where go n
         | x == n    = n
         | otherwise = (+) n $ go (n + 1)
        goNeg n
         | x == n    = n
         | otherwise = (+) n $ goNeg(n - 1)
-- 3.
multBySum :: (Integral a) => a -> a -> a
multBySum x y
  | x == 0 || y == 0 = 0
  | (x < 0 && y < 0)   = go (abs x) (abs y)
  | (x < 0) || (y < 0) = -(go (abs x) (abs y))
  | otherwise          = go x y
  where go a b
         | a == 1    = b
         | otherwise = (+) b $ go (a - 1) b

-- Fixing dividedBy ---------------
-- see eightFive.hs

