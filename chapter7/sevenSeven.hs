module SevenSeven where
  myAbs :: Integer -> Integer
  myAbs x
    | x < 0     = (-x)
    | otherwise = x

  bloodNa :: Integer -> String
  bloodNa x
    | x < 135 = "too low"
    | x > 145 = "too high"
    | otherwise = "just right"

  isRight :: (Num a, Eq a) => a -> a -> a -> String
  isRight a b c
    | a^2 + b^2 == c^2 = "Right On"
    | otherwise        = "not right"

  dogYrs :: Integer -> Integer
  dogYrs x
    | x <= 0 = 0
    | x <= 1 = x * 15
    | x <= 2 = x * 12
    | x <= 4 = x * 8
    | otherwise = x * 6

  avgGrade :: (Fractional a, Ord a) => a -> Char
  avgGrade x
    | y >= 0.9  = 'A'
    | y >= 0.8  = 'B'
    | y >= 0.7  = 'C'
    | y >= 0.59 = 'D'
    | otherwise  = 'F'
    where y = x / 100

  -- Exercises: Gaurd Duty
  -- 1. Done
  -- 2. It type check but will return the wrong values due to specificity
  -- 3. b
  pals :: Eq a => [a] -> Bool
  pals xs
    | xs == reverse xs = True
    | otherwise        = False
  -- 4. A list of values that implement Eq
  -- 5. Eq => [a] -> Bool
  -- 6. c.
  numbers :: (Ord a, Num a, Num p) => a -> p
  numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
  -- 7. Any type that implements Ord and Num
  -- 8.   numbers :: (Ord a, Num a, Num p) => a -> p
