module MonoidComp where

import Test.QuickCheck

type S = String

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (y . x)

instance Monoid (Comp a) where
  mempty = (Comp id)
  mappend = (<>)

-- main :: IO()
-- main = do
--   let ma = monoidAssoc
--       mli = monoidLeftIdentity
--       mlr = monoidRightIdentity
--   quickCheck (ma :: CombineAssoc)
--   quickCheck (mli :: Combine -> Bool)
--   quickCheck (mlr :: Combine -> Bool)