{-# LANGUAGE FlexibleInstances #-}

data Quant a b =
    Finance
  | Desk a
  | Bloor b 
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap f (Bloor b) = Bloor $ f b
  fmap _ (Desk a) = Desk a

data K a b =
  K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap _ (K a) = K a


newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K b)) = Flip $ K $ f b

data EvilGoateeConst a b =
  GoatyConst b 
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a =
  LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut $ fmap f g

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa g h) = DaWrappa (fmap f g) (fmap f h)

data IgnoreOne f g a b = 
  IgnoringSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious x y z) = Notorious x y (fmap f z)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats x y z) =
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print x y) = Print x $ f y 
  fmap f (Read sa) = Read $ fmap f sa


