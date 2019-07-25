{-# LANGUAGE RankNTypes #-}

data List a = 
    Cons a (List a)
  | Nil

x :: List Int
x = Cons 5 $ Cons 6 $ Cons 7 $ Nil

y :: List Int
y = Nil

uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co ni (Cons x xs) = co x xs
uncons co ni Nil         = ni

listNull :: List a -> Bool
listNull = uncons (\_ _ -> False) True

listMap :: (a -> b) -> List a -> List b
listMap f = uncons (\x xs -> Cons (f x) (listMap f xs)) Nil

-- Scott Encoding for a List

newtype ListS a = 
  ListS {
    unconsS :: forall r. (a -> ListS a -> r) -> r -> r
  }

nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a 
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni

instance Functor ListS where
  fmap f = unconsS' (\x xs -> consS (f x) (fmap f xs)) nilS


-- Church Encoding for a List
newtype ListC a = 
  ListC {
    foldC :: forall r. (a -> r -> r) -> r -> r
  }

nilC :: ListC a
nilC = ListC (\co ni -> ni)

consC :: a -> ListC a -> ListC a 
consC x xs = ListC (\co ni -> co x xs)

foldC' :: (a -> r -> r) -> r -> ListC a -> r
foldC' co ni (ListC f) = f co ni

instance Functor ListC where
  fmap f = foldC' (\x xs -> consC (f x) xs) nilC