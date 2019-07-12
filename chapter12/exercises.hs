module Exercises where
notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x     = Just x

replaceThe :: String -> String
replaceThe x = go (words x) ""
  where
    go [] x = x
    go ("the":as) b = go as (b ++ " a")
    go (a:as) b     = go as (b ++ " " ++ x)