module MonoidBoolDisj where

import Test.QuickCheck

type S = String

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype BoolDisj = BoolDisj Bool deriving(Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = (BoolDisj False)
  _ <> _ = (BoolDisj True)

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)


instance Arbitrary BoolDisj where
  arbitrary = oneof [ return $ BoolDisj True,
                      return $ BoolDisj False]

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)