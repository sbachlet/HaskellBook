module SevenEleven where
    -- Chapter Exercises
    -- Multiple Choice -------------------------------
    -- 1. D
    -- 2. B
    test :: Ord a => a -> a -> Bool
    test x y = x > y
    z :: (Ord p, Num p) => p
    z = 5
    test2 :: (Ord a, Num a) => a -> Bool
    test2 = test z
    -- 3. D
    -- 4. B
    -- 5. A
    f :: a -> a
    f x = x

    -- Let's write code -----------------------------------------
    -- 1 a. Ok
    -- 1 b. yes
    tensDigit :: Integral a => a -> a
    -- tensDigit x = d
    --     where xLast = x `div` 10
    --           d     = xLast `mod` 10
    tensDigit = fst . flip divMod 10
    -- 1 c.
    hunsDigit :: Integral a => a -> a
    hunsDigit = fst . flip divMod 100

    -- 2.
    foldBool :: a -> a -> Bool -> a
    foldBool x y z
        | z = y
        | otherwise = x

    foldBool2 :: a -> a -> Bool -> a
    foldBool2 x y z =
        case z of
            True -> y
            False -> x

    -- 3.
    g :: (a -> b) -> (a, c) -> (b, c)
    g f (a, c) = (f a, c)