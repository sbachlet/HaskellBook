module Arbitrary where

import Test.QuickCheck

data Trivial =
    Trivial
    deriving(Eq, Show)

data Identitiy a  = 
    Identitiy a
    deriving (Eq, Show)

identityGen :: Arbitrary a => 
               Gen (Identitiy a)
identityGen = do
  a <- arbitrary
  return (Identitiy a)

instance Arbitrary a =>
         Arbitrary (Identitiy a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identitiy Int)
identityGenInt = identityGen

trivialGen :: Gen Trivial
trivialGen =
    return Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

main :: IO ()
main = do sample trivialGen