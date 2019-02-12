{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module TooManyGoats where
  class TooMany a where
    tooMany :: a -> Bool

  instance TooMany Int where
    tooMany n = n > 42
  instance TooMany (Int, String) where
    tooMany (a, b) = a > 42
  -- instance TooMany (Int, Int) where
  --   tooMany (a, b) = (a + b) > 42

  newtype Goats =
    Goats Int deriving (Eq, Show)

  instance TooMany Goats where
    tooMany (Goats n) = tooMany n

  instance TooMany (Goats, Goats) where
    tooMany (Goats n, Goats m) = tooMany (n, m)

  instance (Num a, TooMany a) =>  TooMany (a, a) where
    tooMany (n, m) = (tooMany (n + m))