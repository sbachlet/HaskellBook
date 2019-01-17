module StandardFunctions where

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs)
  | x == True = True
  | otherwise = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
  | f x == True = True
  | otherwise   = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
  | x == y    = True
  | otherwise = myElem x ys

myElem1 :: Eq a => a -> [a] -> Bool
myElem1 x = any (== x)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ (squish xs)

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = x ++ squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) =
  go f xs x
  where go g (y:ys) z
         | f y z     == GT = go f ys y
         | otherwise       = go f ys z
        go _ [] z          = z

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) =
  go f xs x
  where go g (y:ys) z
         | f y z     == LT = go f ys y
         | otherwise       = go f ys z
        go _ [] z          = z

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare