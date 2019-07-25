module StringProcessing where

    -- replaceThe :: String -> String
    replaceThe = unwords . (map swapLetter) . (map notThe) . words
        
    swapLetter :: Maybe String -> String
    swapLetter (Just a) = a
    swapLetter _        = "a"

    notThe :: String -> Maybe String
    notThe x
        | x == "the" = Nothing
        | otherwise  = Just x 


    countTheBeforeVowel :: String -> Int
    countTheBeforeVowel = countVowelThe . (map notThe). words
    
    countVowelThe :: [Maybe String] -> Int
    countVowelThe []       = 0
    countVowelThe (x:[])   = 0
    countVowelThe (x:y:[]) = checkVowelFold x y
    countVowelThe (x:y:xs) = (checkVowelFold x y) + (countVowelThe ([y] ++ xs))

    isVowelWord :: Char -> Int
    isVowelWord 'a' = 1
    isVowelWord 'e' = 1
    isVowelWord 'i' = 1
    isVowelWord 'o' = 1
    isVowelWord 'u' = 1
    isVowelWord _   = 0

    checkVowelFold :: Maybe String -> Maybe String -> Int
    checkVowelFold Nothing (Just (x:_)) = isVowelWord x
    checkVowelFold _ _                  = 0

    newtype Word' = 
        Word' String
        deriving(Eq, Show)

    vowels = "aeiou"

    mkWord :: String -> Maybe Word'
    mkWord x = if n >= 0 then Nothing else Just (Word' x)
        where 
            n = foldr (\y z -> z + isVowel y) 0 x

    isVowel :: Char -> Int
    isVowel x 
        | x == ' '      = 0
        | elem x vowels = 1
        | otherwise     = (-1) 