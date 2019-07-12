{-# LANGUAGE RankNTypes #-}

type Nat f g = forall a . f a -> g a 

maybeToList :: Nat Maybe (Either Int)
maybeToList Nothing = Left 0
maybeToList (Just a) = Right a

