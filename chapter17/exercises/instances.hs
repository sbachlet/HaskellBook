import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Begin Pair
data Pair a = Pair a a deriving(Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

instance (Semigroup a) => Semigroup (Pair a) where
  (<>) (Pair a a') (Pair b b') = Pair (a <> b) (a' <> b')

instance (Monoid a) => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)

instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f f') (Pair a a') = Pair (f a) (f' a')

instance Eq a => EqProp (Pair a) where
  (=-=) = eq
-- End Pair

-- Begin Two
data Two a b = Two a b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a a') (Two b b') = Two (a <> b) (a' <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance Monoid a => Applicative (Two a) where
  pure = Two mempty
  (<*>) (Two f f') (Two a a') = Two (f <> a) (f' a')

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

-- End Two

-- Begin Three
data Three a b c = Three a b c deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>  Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure = Three mempty mempty
  (<*>) (Three f g h) (Three a b c) = Three (f <> a) (g <> b) (h c)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
-- End Three

-- Begin Three'
data Three' a b = Three' a b b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Semigroup a, Semigroup b) => Semigroup (Three' a b) where
  (<>) (Three' a b c) (Three' a' b' c') = Three' (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b) => Monoid (Three' a b) where
  mempty = Three' mempty mempty mempty
  mappend = (<>)

instance (Monoid a) => Applicative (Three' a) where
  pure b = Three' mempty b b
  (<*>) (Three' f g g') (Three' a b b') = Three' (f <> a) (g b) (g b')
  
instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- End Three'

--Begin Four
data Four a b c d = Four a b c d deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b,  Arbitrary c, Arbitrary d) => 
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  mappend = (<>)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  (<*>) (Four f g h i) (Four a b c d) = Four (f <> a) (g <> b) (h <> c) (i d)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
-- End Four 

-- Begin Four'
data Four' a b = Four' a a a b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
  (<>) (Four' a0 a1 a2 b) (Four' a0' a1' a2' b') = Four' (a0 <> a0') (a1 <> a1') (a2 <> a2') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty = Four' mempty mempty mempty mempty
  mappend = (<>)

instance Monoid a => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  (<*>) (Four' f0 f1 f2 g) (Four' a0 a1 a2 b) = Four' (f0 <> a0) (f1 <> a1) (f2 <> a2) (g b)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
-- End Four'

x :: (Sum Int, String, Product Int)
x = (1, "a", 2)

stops :: [Char]
stops = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

stopVowelStop = liftA3 (\x y z -> x : y : z : []) stops vowels stops

main :: IO ()
main = do
  quickBatch $ functor (Pair x x)
  quickBatch $ monoid ((Pair "Test" "Test"))
  quickBatch $ applicative (Pair x x)

  quickBatch $ functor (Two x x)
  quickBatch $ monoid ((Two "Test" "Test"))
  quickBatch $ applicative (Two x x)

  quickBatch $ functor (Three x x x)
  quickBatch $ monoid ((Three "Test" "Test" "Test"))
  quickBatch $ applicative (Three x x x) 

  quickBatch $ functor (Three' x x x)
  quickBatch $ monoid ((Three' "Test" "Test" "Test"))
  quickBatch $ applicative (Three' x x x) 

  quickBatch $ functor (Four x x x x)
  quickBatch $ monoid ((Four "Test" "Test" "Test" "Test"))
  quickBatch $ applicative (Four x x x x)
  
  quickBatch $ functor (Four' x x x x)
  quickBatch $ monoid ((Four' "Test" "Test" "Test" "Test"))
  quickBatch $ applicative (Four' x x x x) 