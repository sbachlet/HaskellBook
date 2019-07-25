module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- instance Semigroup a => Semigroup (ZipList a) where
--   (<>) = liftA2 (<>)

-- instance Monoid a => Monoid (ZipList a) where
--   mempty = pure mempty
--   mappend = (<>)

-- instance Eq a => EqProp (ZipList a) where
--   (=-=) = eq

data List a = 
    Nil 
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b)
        -> (List a)
        -> (List b)
flatMap f as = concat' $ fmap f as

instance Semigroup (List a) where
  (<>) Nil ys = ys
  (<>) ys Nil = ys
  (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Monoid (List a) where
  mappend = (<>)
  mempty = Nil

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) a = (f <$> a) <> (fs <*> a) 

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq
    
take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = Cons a (take' (n-1) as)

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = 
    ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' Nil)           _                      = ZipList' Nil
  (<*>) _                       (ZipList' Nil)          = ZipList' Nil
  (<*>) (ZipList' (Cons f fs))  (ZipList' (Cons a Nil)) = ZipList' (Cons (f a) (fs <*> pure a))
  (<*>) (ZipList' (Cons f Nil)) (ZipList' (Cons a as))  = ZipList' (Cons (f a) (f <$> as))
  (<*>) (ZipList' (Cons f fs))  (ZipList' (Cons a as))  = ZipList' (Cons (f a) (fs <*> as))

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a

quickList = Cons (([1], "s", 'c') :: ([Int], String, Char)) Nil

main :: IO ()
main = do
  quickBatch $ monoid quickList
  quickBatch $ applicative quickList
  quickBatch $ applicative $ ZipList' quickList