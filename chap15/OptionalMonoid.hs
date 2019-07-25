module OptionalMonoid where

import Data.Monoid

data Optional a = 
    Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  (Only a) <> Nada = Only $ a
  Nada <> (Only b) = Only $ b
  (Only a) <> (Only b) = Only $ a <> b
  Nada <> Nada = Nada

newtype First' a =
  First' {getFirst' :: Optional a}
  deriving(Eq, Show)

instance Monoid a => Monoid (First' a) where
  mempty = First' {getFirst' = Nada}

instance Semigroup a => Semigroup (First' a) where
  (First' (Only a)) <> _ = First' {getFirst' = Only a}
  _ <> (First' (Only a')) = First' {getFirst' = Only a'}
  _ <> _ = First' {getFirst' = Nada}

main :: IO ()
main = do
  print $ Only (Sum 1) <> Only (Sum 1)
  print $ Only (Sum 1) <> Nada
  print $ Only [1] <> Nada
  print $ Nada <> Only (Sum 1)