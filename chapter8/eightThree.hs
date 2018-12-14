module EightThree where

f :: Bool -> Maybe Int
f False = Just 0
f _ = Nothing

-- data Maybe a = Nothing | Just a
