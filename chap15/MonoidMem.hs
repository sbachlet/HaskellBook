module MonoidMem where

newtype Mem s a = 
  Mem {
    runMem :: s -> (a, s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (Mem x) <> (Mem y) = Mem(\z -> 
    let (a,b) = y z
        (c,d) = x b
    in (a <> c, d))

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print $ rmleft
  print $ rmright
  print $ (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0