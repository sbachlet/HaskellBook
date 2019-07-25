import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

-- Begin Identity

newtype Identity a = Identity a deriving(Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Arbitrary a => Arbitrary (Identity a) where 
  arbitrary = do
    a <- arbitrary
    return $ Identity a

identityIdentity :: Identity Int -> Bool
identityIdentity x = functorIdentity x
    
type IdFC = Identity Int -> IntToInt -> IntToInt -> Bool

-- End Identity 
-- Begin Pair
data Pair a = Pair a a deriving(Eq, Show)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a

instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')

pairIdentity :: Pair Int -> Bool
pairIdentity x = functorIdentity x

type PairFC = Pair Int -> IntToInt -> IntToInt -> Bool
-- End Pair

-- Begin Two
data Two a b = Two a b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

twoIdentity :: Two Int Int -> Bool
twoIdentity x = functorIdentity x

type TwoFC = Two Int Int -> IntToInt -> IntToInt -> Bool
-- End Two
-- Begin Three
data Three a b c = Three a b c deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>  Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance Functor (Three a b) where 
  fmap f (Three a b c) = Three a b (f c)

threeIdentity :: Three Int Int Int -> Bool
threeIdentity x = functorIdentity x

type ThreeFC = Three Int Int Int -> IntToInt -> IntToInt -> Bool
-- End Three

-- Begin Three'
data Three' a b = Three' a b b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

three'Identity :: Three' Int Int -> Bool
three'Identity x = functorIdentity x

type Three'FC = Three' Int Int -> IntToInt -> IntToInt -> Bool
-- End Three'

--Begin Four
data Four a b c d = Four a b c d deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b,  Arbitrary c, Arbitrary d) => 
  Arbitrary (Four a b c d) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      d <- arbitrary
      return $ Four a b c d

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

fourIdentity :: Four Int Int Int Int -> Bool
fourIdentity x = functorIdentity x

type FourFC = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool
-- End Four 

-- Begin Four'
data Four' a b = Four' a a a b deriving(Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

four'Identity :: Four' Int Int -> Bool
four'Identity x = functorIdentity x

type Four'FC = Four' Int Int  -> IntToInt -> IntToInt -> Bool
-- End Four'

-- Begin Trivial
 
data Trivial = Trivial deriving(Eq, Show)

-- There is no valid instance of Functor for Trivial since Trivial is not higher kinded
instance Arbitrary Trivial where
  arbitrary = return $ Trivial

-- End Trivial

main :: IO ()
main = do
  quickCheck identityIdentity
  quickCheck (functorCompose' :: IdFC)

  quickCheck pairIdentity
  quickCheck (functorCompose' :: PairFC)

  quickCheck twoIdentity
  quickCheck (functorCompose' :: TwoFC)

  quickCheck threeIdentity
  quickCheck (functorCompose' :: ThreeFC)

  quickCheck three'Identity
  quickCheck (functorCompose' :: Three'FC)

  quickCheck fourIdentity
  quickCheck (functorCompose' :: FourFC)

  quickCheck four'Identity
  quickCheck (functorCompose' :: Four'FC)

  