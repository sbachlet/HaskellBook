module ChapterSix where
  import Data.List

  data DayOfWeek =
    Mon | Tue | Weds | Thu | Fri | Sat | Sun
    deriving (Show)

  instance Eq DayOfWeek where
    (==) Mon Mon   = True
    (==) Tue Tue   = True
    (==) Weds Weds = True
    (==) Thu Thu   = True
    (==) Fri Fri   = True
    (==) Sat Sat   = True
    (==) Sun Sun   = True
    (==) _ _       = False

  instance Ord DayOfWeek where
    compare Fri Fri = EQ
    compare Fri _   = GT
    compare _ Fri   = LT
    compare _ _     = EQ

  -- data Mood = Blah

  -- instance Show Mood where
    -- show _ = "Blah"

  class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer

  newtype Age =
    Age Integer
    deriving(Eq, Show)

  instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n

  newtype Year =
    Year Integer
    deriving(Eq, Show)

  instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n

  sumNumberish :: Numberish a => a -> a -> a
  sumNumberish a a' = fromNumber summed
    where integerOfA      = toNumber a
          integerOfAPrime = toNumber a'
          summed =
            integerOfA + integerOfAPrime

  add ::  Int -> Int -> Int
  add x y = x + y

  addWeird :: Int -> Int -> Int
  addWeird x y =
    if x > 1
    then x + y
    else x

  check' :: Int -> Int -> Bool
  check' a a' = a == a'

  -- 6.14 Multiple Choice
  -- 1. c
  -- 2. b
  -- 3. a
  -- 4. c
  -- 5. a

  -- 6.14 Does it typecheck?
  -- 1. Did not typecheck needed instance of Show
  data Person = Person Bool deriving(Show)

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  --2. Did not type check needed instance of Eq
  data Mood = Blah
            | Woot deriving  Show

  instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _       = False

  settleDown x = if x == Woot
                    then Blah
                    else x

  -- 3.a Blah or Woot
  -- 3.b It will fail with an exception due to 9 not being a valid type for the typeclass
  -- 3.c It will fail with an exception due to Mood not having an instance of the Ord typeclass

  -- 4. Type checks s1 is just partialy applied
  type Subject = String
  type Verb = String
  type Object = String

  data Sentence =
    Sentence Subject Verb Object
    deriving (Eq, Show)

  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  -- 16.4 Given a datatype declaration, what can we do?
  data Rocks =
    Rocks String deriving(Eq, Show, Ord)

  data Yeah =
    Yeah Bool deriving(Eq, Show, Ord)

  data Papu =
    Papu Rocks Yeah
    deriving(Eq, Show, Ord)

  -- 1. The arguments lacked sufficient type constraints
  phew = Papu (Rocks "chases") (Yeah True)

  -- 2. This is fine
  truth = Papu (Rocks "chomskydoz") (Yeah True)

  -- 3. This is fine since Papu has an Eq type class instance
  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

  -- 4. This fails since Papu does not have a Ord type class instance
  comaperPapus :: Papu -> Papu -> Bool
  comaperPapus p p' = p > p'

  -- 16.4 Match the types
  -- 1. No
  i :: Num a => a
  -- i :: a
  i = 1

  -- 2. No
  f :: Float
  -- f :: Num a => a 
  f = 1.0

  -- 3. Yes
  -- g :: Float
  g :: Fractional a => a
  g = 1.0

  -- 4. Yes
  -- h :: Float 
  h :: RealFrac a => a
  h = 1.0

  -- 5. Yes
  -- freud :: a -> a
  freud :: Ord a => a  -> a
  freud x = x
  
  -- 6. Yes
  -- freud' :: a -> a
  freud' :: Int -> Int
  freud' x = x

  -- 7. No
  myX = 1 :: Int
  sigmund :: Int -> Int
  -- sigmund :: a -> a
  sigmund x = myX

  -- 8. No
  myX' = 1 :: Int
  sigmund' :: Int -> Int
  -- sigmund' :: Num a => a -> a
  sigmund' x = myX'

  -- 9. Yes because Int has an Instance of Ord
  -- jung :: Ord a => [a] -> a
  jung :: [Int] -> Int
  jung xs = head (sort xs) 

  -- 10. Yes because the type definition of sort requires any types
  --     that has an instance of Ord
  -- young :: [Char] -> Char
  young :: Ord a => [a] -> a
  young xs = head (sort xs)

  -- 11. No because the type definition of mySort restricts the type of the sort
  mySort :: [Char] -> [Char]
  mySort = sort

  signifier :: [Char] -> Char
  -- signifier :: Ord a => [a] -> a
  signifier xs = head (mySort xs)

  -- 6.14 Type-Know-Do Two:: Electric Typealoo
  -- 1. 
  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk fn x y = fn x == y
  
  -- 2. 
  arith :: Num b
        => (a -> b)
        -> Integer
        -> a
        -> b
  arith fn x y = (fn y) + (fromInteger x)  


















































