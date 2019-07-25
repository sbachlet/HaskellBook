
import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation' e a =
    Failure' e
  | Success' a
  deriving (Eq, Show)

instance Functor (Validation' e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' $ f a

instance (Semigroup e, Semigroup a) => Semigroup (Validation' e a) where
  (<>) (Success' a) (Failure' e) = Failure' e
  (<>) (Failure' e) (Success' a) = Failure' e
  (<>) (Success' a) (Success' a') = Success' $ a <> a'
  (<>) (Failure' e) (Failure' e') = Failure' $ e <> e'

instance (Monoid e, Monoid a) => Monoid (Validation' e a) where
  mappend = (<>)
  mempty = Success' $ mempty

instance (Monoid e) => Applicative (Validation' e) where
  pure = Success'
  (<*>) (Success' f) (Success' a) = Success' $ f a
  (<*>) (Success' f) (Failure' e) = Failure' e
  (<*>) (Failure' e) (Success' f) = Failure' e 
  (<*>) (Failure' e) (Failure' e') = Failure' $ e <> e'

instance (Arbitrary a, Arbitrary e) => Arbitrary (Validation' e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    oneof [ return $ Success' a
          , return $ Failure' e ]

instance (Eq e, Eq a) => EqProp (Validation' e a) where
  (=-=) = eq

main :: IO ()
main = do 
  quickBatch $ monoid (Success' ["Test"] :: Validation' ([Int], [Int], [Int]) [String])
  quickBatch $ functor (Success' (1,2,3) :: Validation' ([Int], [Int], [Int]) (Int, Int, Int))
  quickBatch $ applicative (Failure' ["Test"] :: Validation' [String] ([Int], [Int], [Int]))



