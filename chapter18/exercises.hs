import Test.QuickCheck 
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid (Sum, Product)
import Control.Monad (join, liftM2)
import Control.Applicative

data Nope a = 
  NopeDotJpg
  deriving (Eq, Show)

instance Semigroup (Nope a) where
  NopeDotJpg <> NopeDotJpg = NopeDotJpg

instance Monoid (Nope a) where
  mempty = NopeDotJpg
  mappend = (<>)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure a = NopeDotJpg
  NopeDotJpg <*> NopeDotJpg = NopeDotJpg

instance Monad Nope where
  return = pure
  NopeDotJpg >>= f = NopeDotJpg
instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

nopeTest :: Nope (Sum Int, String, Product Int)
nopeTest = undefined

data PhhhbbtttttEither b a =
    Left' a
  | Right' b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (PhhhbbtttttEither b a) where
  Left' a <> Left' a' = Left' $ a <> a'
  Left' a <> Right' _ = Left' a
  Right' _ <> Left' a = Left' a
  Right' b <> Right' b' = Right' $ b <> b'

instance (Monoid a, Monoid b) => Monoid (PhhhbbtttttEither b a) where
  mempty = Right' mempty
  mappend = (<>)

instance Functor (PhhhbbtttttEither b) where
  fmap f (Left' a) = Left' $ f a
  fmap _ (Right' b) = Right' b
  
instance Monoid b => Applicative (PhhhbbtttttEither b) where
  pure = Left'
  Left' f <*> Left' a = Left' $ f a
  Right' b <*> Left' a = Right' b
  Left' a <*> Right' b = Right' b
  Right' b <*> Right' b' = Right' $ b <> b'

instance Monoid b => Monad (PhhhbbtttttEither b) where
  return = pure
  Left' a >>= f = f a
  Right' b >>= _ = Right' b

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttttEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [ return $ Left' a
          , return $ Right' b]
  
instance (Eq b, Eq a) => EqProp (PhhhbbtttttEither b a) where
  (=-=) = eq

phbtEitherTest :: PhhhbbtttttEither String (Sum Int, String, Product Int)
phbtEitherTest = undefined

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity $ a <> a'

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity $ f a

instance Monad Identity where
  return = pure
  Identity a >>= f = f a
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do 
    a <- arbitrary
    return $ Identity a

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityTest :: Identity (Sum Int, String, Product Int)
identityTest = undefined 

data List a = 
    Nil 
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) (fmap f as)

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


fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold (<>) Nil

flatMap :: (a -> List b)
        -> (List a)
        -> (List b)
flatMap f as = concat' $ fmap f as

instance Monad List where
  return = pure
  a >>= f =  flatMap f a

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]

instance Eq a => EqProp (List a) where
  (=-=) = eq

listTest :: List (Sum Int, String, Product Int)
listTest = undefined

j :: Monad m => m (m a) -> m a
-- j = join
j mma = do
  ma <- mma
  ma

l1 :: Monad m => (a -> b) -> m a -> m b
-- l1 = (<$>)
l1 f ma = do
  a <- ma
  return $ f a

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 = liftM2
l2 f ma mb = do
  a <- ma
  b <- mb
  return $ f a b 

a :: Monad m => m a -> m (a -> b) -> m b
-- a = flip (<*>)
a ma mf = do
  a <- ma
  f <- mf
  return $ f a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (x:xs) f = liftM2 (<>) ((:[]) <$> (f x)) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType x = meh x id

main :: IO ()
main = do
  quickBatch $ monoid nopeTest
  quickBatch $ functor nopeTest
  quickBatch $ applicative nopeTest
  quickBatch $ monad nopeTest

  quickBatch $ monoid phbtEitherTest
  quickBatch $ functor phbtEitherTest
  quickBatch $ applicative phbtEitherTest
  quickBatch $ monad nopeTest

  quickBatch $ monoid identityTest
  quickBatch $ functor identityTest
  quickBatch $ applicative identityTest
  quickBatch $ monad identityTest

  quickBatch $ monoid listTest
  quickBatch $ functor listTest 
  quickBatch $ applicative listTest
  quickBatch $ monad listTest
