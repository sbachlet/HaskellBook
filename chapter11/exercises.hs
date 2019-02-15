module Exercises where
import Data.Char
capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\w@(x:xs) -> (w, toUpper x:xs)) . words

capitalizeParagraph :: String -> String
capitalizeParagraph x = go . capitalizeWord $ x
  where
    go []            = []
    go ('.': ' ':xs) = ". " ++ (go $ capitalizeWord xs)
    go (x:xs)        = x : go xs