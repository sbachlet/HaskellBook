module Vigenere where
import Data.Char

type Key = String
type PlainText = String
type EncodedText = String

chrToNum :: Char -> Int
chrToNum = flip (-) (ord 'a') . ord

numToChr :: Int -> Char
numToChr = chr . (+) 97 . flip mod 26

vigenere :: PlainText -> Key -> EncodedText
vigenere ptext key = map numToChr . (zipWith (\x y -> (+) x y) (map chrToNum ptext) (map chrToNum (cycle key)))