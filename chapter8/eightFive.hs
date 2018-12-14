module EightFive where
newtype Dividend =
  Dividend Integer
  deriving(Eq, Show)

newtype Remainder =
  Remainder Integer
  deriving(Eq, Show)

data DividedResult =
    Result (Dividend, Remainder)
  | DividedByZero
  deriving(Eq, Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy _ 0 = DividedByZero
dividedBy num denom =
  go (abs num) (abs denom) 0
  where go n   d count
         | n < d && num > 0 && denom > 0 = (Result (Dividend count,    Remainder n))
         | n < d && num < 0 && denom < 0 = (Result (Dividend count,    Remainder n))
         | n < d && num < 0              = (Result (Dividend (-count), Remainder n))
         | n < d &&            denom < 0 = (Result (Dividend (-count), Remainder (-n)))
         | otherwise =
             go (n - d) d (count + 1)