module Phone where
  import Data.Char
  import Data.List

  data Button = Button Digit String deriving(Eq, Show)
  data DaPhone = DaPhone [Button] deriving (Eq, Show)
  phone :: DaPhone
  phone = DaPhone [
    Button '1' "1",
    Button '2' "abc2",
    Button '3' "def3",
    Button '4' "ghi4",
    Button '5' "jkl5",
    Button '6' "mno6",
    Button '7' "pqrs7",
    Button '8' "tuv8",
    Button '9' "wxyz9",
    Button '*' "^",
    Button '0' "+ 0",
    Button '#' ".,"]

  convo :: [String]
  convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]

  -- -- validButtons = "1234567890*#"
  type Digit = Char

  -- -- Valid presses: 1 and up
  type Presses = Int

  lookupButton :: DaPhone -> Char -> Button
  lookupButton (DaPhone buttons) char =
      head
    . filter (\(Button _ x) -> elem (toLower char) x) $ buttons

  digit :: Button -> Digit
  digit (Button x _) = x

  numTap :: Button -> Char -> Int
  numTap (Button _ y) char = go y 0
    where
      go [] n = n
      go (x : xs) n
       | x == char = n+1
       | otherwise = go xs $ n+1

  reverseTaps :: DaPhone
              -> Char
              -> [(Digit, Presses)]
  reverseTaps daPhone char = upper ++ digitLookup
    where
      button = lookupButton daPhone char
      upper = if isUpper char then [('*', 1)] else []
      digitLookup  = [(digit $ button, numTap button (toLower char))]


  -- -- assuming the default phone definition
  -- -- 'a' -> [('2', 1)]
  -- -- 'A' -> [('*', 1), ('2', 1)]
  cellPhonesDead :: DaPhone
                 -> String
                 -> [(Digit, Presses)]
  cellPhonesDead daPhone = foldr (\x -> (++) $ reverseTaps daPhone x) []

  fingerTaps :: [(Digit, Presses)] -> Presses
  fingerTaps = foldr (\(_, x) -> (+) x) 0

  mostPopularLetter :: String -> Char
  mostPopularLetter =
    head
    . maximumBy (\x y -> compare (length x) (length y))
    . group
    . sort
    . filter (\x -> x /= ' ')

  coolestLtr :: [String] -> Char
  coolestLtr =
    mostPopularLetter
    . map mostPopularLetter

  coolestWord :: [String] -> String
  coolestWord =
    head
    . maximumBy (\x y -> compare (length x) (length y))
    . group
    . sort
    . words
    . map toLower
    . foldr (\x -> (++) x) ""