module ThyFearfulSymmetry where
-- 1.
myWords :: String -> Char -> [String]
myWords [] _ = []
myWords x y = (:) (takeWhile (/=y) x) (myWords (drop 1 . dropWhile (/=y) $ x ) y)

-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
           \ symmetry?"
sentences = firstSen ++ secondSen
          ++ thirdSen ++ fourthSen
-- putStrLn sentences -- should print
-- Tyger Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?
-- Implement this
myLines :: String -> Char -> [String]
myLines [] _ = []
myLines x y = (:) (takeWhile (/= y) x) (myLines (drop 1 . dropWhile (/= y) $ x ) y)

-- What we want 'myLines sentences'
-- to equal
shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

-- The main function here is a small test
-- to ensure you've written your function
-- correctly.
main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences '\n'
           == shouldEqual)