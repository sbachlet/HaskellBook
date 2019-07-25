module Validation where

data Validation a b =
  Failure a | Success b
  deriving(Eq, Show)

instance Semigroup a => 
  Semigroup (Validation a b) where
    (Failure x) <> (Failure y) = Failure $ x <> y
    (Success a) <> _           = Success a
    _ <> (Success a)           = Success a


main :: IO ()
main = do
  let failure :: String
                -> Validation String Int
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2

