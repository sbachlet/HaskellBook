import Control.Applicative

f :: Int -> Maybe String
f x = lookup x [ (3, "hello")
               , (4, "julie")
               , (5, "kbai")]

g :: Int -> Maybe String
g y = lookup y [ (7, "sup?")
               , (8, "chris")
               , (9, "aloha")]

h :: Int -> Maybe Int
h z = lookup z [(2,3), (5,6), (7,8)]

m :: Int -> Maybe Int
m x = lookup x [(4,10), (8,13), (1,9001)]

main :: IO ()
main = do 
  print $ f 3
  print $ g 8
  print $ liftA2 (++) (f 3) (g 7)
  print $ liftA2 (+) (h 5) (m 1)
  print $ liftA2 (+) (h 5) (m 6)