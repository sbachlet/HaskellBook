import Control.Applicative
import Data.List (elemIndex)

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

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer 
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer 
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x' :: Maybe Int
x' = elemIndex 3 [1,2,3,4,5]

y' :: Maybe Int
y' = elemIndex 4 [1,2,3,4,5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x' <*> y'

xs = [1,2,3]
ys = [4,5,6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed =  sum <$> ((,) <$> x'' <*> y'')

main :: IO ()
main = do 
  print $ f 3
  print $ g 8
  print $ (++) <$> (f 3) <*> (g 7)
  print $ (+) <$> (h 5) <*> (m 1)
  print $ (+) <$> (h 5) <*> (m 6)
  print $ liftA2 (++) (g 9) (f 4)
  print $ liftA2 (^) (h 5) (m 4)
  print $ liftA2 (*) (h 5) (m 4)
  print $ liftA2 (*) (h 1) (m 1)
  -- a <- fmap length $ liftA2 (++) getLine getLine
  -- b <- liftA2 (,) getLine getLine
  -- print $ a
  -- print $ b
  print added
  print tupled
  print maxed
  print summed

