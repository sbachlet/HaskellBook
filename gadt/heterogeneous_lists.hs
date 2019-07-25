{-# LANGUAGE ExistentialQuantification #-}

data ShowBox = forall s. Show s => SB s

heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

instance Show ShowBox where
  show (SB s) = show s

f :: [ShowBox] -> IO ()
f xs = mapM_ print xs

data T = forall a. MkT a

-- MkT :: forall a. (a -> T)

-- heteroList' = [MkT 5, MkT (), MkT True, MkT map]
data T' = forall a. Show a => MkT' a
heteroList' :: [T']
heteroList' = [MkT' 5, MkT' (), MkT' True, MkT' "Sartre"]

main = mapM_ (\(MkT' x) -> print x) heteroList'