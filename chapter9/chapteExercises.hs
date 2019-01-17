module ChapterExercises where
import Data.Char
-- Data.Char
-- 1.
-- isUpper :: Char -> Bool
-- toUpper :: Char -> Char
-- 2.
two :: String -> String
two x = filter isUpper x
-- 3
three :: String -> String
three (x:xs) = toUpper x : xs
-- 4
four :: String -> String
four "" = ""
four (x:xs) = toUpper x : four xs

-- 5
five :: String -> Char
five x = toUpper . head $ x

-- 6
six :: String -> Char
six = toUpper . head

