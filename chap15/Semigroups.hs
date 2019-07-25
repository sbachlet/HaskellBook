module Semigroups where

import Test.QuickCheck

type S = String

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = 
  Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving(Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity $ a <> a'

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving(Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc =
  Two S S -> Two S S -> Two S S -> Bool 

data Three a b c = Three a b c deriving(Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc =
  Three S S S -> Three S S S -> Three S S S -> Bool 

data Four a b c d = Four a b c d deriving(Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four w x y z) <> (Four w' x' y' z') = Four (w <> w') (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc =
  Four S S S S-> Four S S S S-> Four S S S S-> Bool 

newtype BoolConj = BoolConj Bool deriving(Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = (BoolConj True)
  _ <> _                             = (BoolConj False)

instance Arbitrary BoolConj where
  arbitrary = oneof [ return $ BoolConj True,
                      return $ BoolConj False]

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving(Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
  _ <> _ = (BoolDisj True)

instance Arbitrary BoolDisj where
  arbitrary = oneof [ return $ BoolDisj True,
                      return $ BoolDisj False]

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst a') = (Fst a)
  (Fst a) <> (Snd b) = (Snd b)
  (Snd a) <> (Fst b) = (Snd a)
  (Snd a) <> (Snd a') = (Snd a)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Fst a,
           return $ Snd b]

type OrAssoc = 
  Or S S -> Or S S -> Or S S -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (y . x)

data Validation a b =
  Failure a | Success b
  deriving(Eq, Show)

-- instance Semigroup a => 
--   Semigroup (Validation a b) where
--     (<>) = undefined


main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeAssoc)
  quickCheck (semigroupAssoc :: FourAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrAssoc)
  let failure :: String
                -> Validation String Int
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
