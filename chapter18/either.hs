module EitherMonad where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import qualified Data.Monoid as DM

type Founded = Int
type Coders = Int

data SoftwareShop = 
  Shop {
      founded :: Founded
    , programmers :: Coders
  } deriving (Eq, Show)

data FoundedError = 
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n 
  | n < 0 = Left $ NegativeYears n
  | n > 500 = Left $ TooManyYears n
  | otherwise = Right n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0 = Left $ NegativeCoders n
  | n > 5000 = Left $ TooManyCoders n
  | otherwise = Right n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders
  if programmers > div founded 10
    then Left $ 
            TooManyCodersForYears
            founded programmers
    else Right $ Shop founded programmers

data Sum a b = 
    First a 
  | Second b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Sum a b) where
  (<>) (First a) (First a') = First $ a <> a'
  (<>) (Second b) (Second b') = Second $ b <> b'
  (<>) (First a) (Second b) = First a
  (<>) (Second b) (First a) = First a

instance (Monoid a, Monoid b) => Monoid (Sum a b) where
  mempty = Second mempty
  mappend = (<>)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second $ f b

instance (Monoid a) => Applicative (Sum a) where
  pure = Second
  (<*>) (First a) (Second _) = First a
  (<*>) (Second _) (First a) = First a
  (<*>) (Second f) (Second a) = Second $ f a
  (<*>) (First a) (First a') = First $ a <> a'

instance Monoid a => Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = (First a)
  (>>=) (Second b) f = f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ First a
          , return $ Second b ]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq


main = do
  let testTrigger :: Sum String ((DM.Sum Int), String, (DM.Product Int))
      testTrigger = undefined
  quickBatch $ monoid testTrigger
  quickBatch $ functor testTrigger
  quickBatch $ applicative testTrigger
  quickBatch $ monad testTrigger
