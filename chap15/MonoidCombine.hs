module MonoidCombine where

import Test.QuickCheck

type S = String

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = (Combine mempty)
  mappend = (<>)

-- main :: IO()
-- main = do
--   let ma = monoidAssoc
--       mli = monoidLeftIdentity
--       mlr = monoidRightIdentity
--   quickCheck (ma :: CombineAssoc)
--   quickCheck (mli :: Combine -> Bool)
--   quickCheck (mlr :: Combine -> Bool)