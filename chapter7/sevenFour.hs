-- Exercises: Variety Pack
module SevenFour where
    -- 1.a
    k :: (x, y) -> x
    k (x, y) = x
    k1 = k (((-) 4 1), 10)
    -- 1.b No, k1 and k3 are the same but k2 is a [Char]
    k2:: [Char]
    k2 = k ("three", ((+) 1 2))
    k3 = k (3, True)
    -- 1.c k1 and k3 will return 3.
    -- 2
    f :: (a, b, c)
      -> (d, e, f)
      -> ((a, d), (c, f))
    f (a, b, c) (d, e, f) = ((a, d), (c, f))