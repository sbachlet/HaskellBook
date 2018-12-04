module SevenThree where
    -- Exercises: Grab Bag
    -- 1. They're all equivalent
    -- mTh x y z = x * y * z
    -- mTh x y = \z -> x * y * z
    mTh x = \y -> \z -> x * y * z
    -- mTh = \x -> \y -> \z -> x * y * z
    -- 2. b
    -- 3.
    -- a.
    addOneIfOdd n = case odd n of
        True -> f n
        False -> n
        where f = \n -> n + 1
    -- b.
    addFive = \x -> \y -> (if x > y then y else x) + 5
    -- c.
    mflip f x y = f y x