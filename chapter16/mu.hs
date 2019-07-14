newtype Mu f = Inf { outF :: f (Mu f)}

instance Functor Mu where
  fmap = undefined