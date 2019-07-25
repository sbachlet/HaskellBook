module MonoidBoolConj where

import Test.QuickCheck

type S = String

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = 
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype BoolConj = BoolConj Bool deriving(Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = (BoolConj True)
  _ <> _                             = (BoolConj False)

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

instance Arbitrary BoolConj where
  arbitrary = oneof [ return $ BoolConj True,
                      return $ BoolConj False]

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO()
main = do
  let ma = monoidAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (ma :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)