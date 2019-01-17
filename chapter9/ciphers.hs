module Cipher where
import Data.Char

caesar :: Int -> String -> String
caesar x = map (shift x)

shift :: Int -> Char -> Char
shift y x
  | ord x > 64 && ord x < 91  = chr $ (+) 65 $ mod (ord x + y - 65) 26
  | ord x > 96 && ord x < 123 = chr $ (+) 97 $ mod (ord x + y - 97) 26
  | otherwise                 = x

unCaesar :: Int -> String -> String
unCaesar x = caesar (-x)