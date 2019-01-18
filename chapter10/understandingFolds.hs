module UnderstandingFolds where
  -- 1. C
  --2. 3 * (2 * (1 * 1))
  --   3 * (2 * 1)
  --   3 * 2
  --   6
  -- 3. C
  -- 4. A
  -- 5.
  foldr (++) "" ["woot", "WOOT", "woot"]
  foldr max ' ' "fear is the little death"
  foldr (&&) True [False, True]
  foldr (||) False [False, True]
  foldl (\x -> ((++) x) . show) "" [1..5]
  foldr const 0 [1..5]
  foldr const 'a' "tacos"
  foldl (flip const) 0 [1..5]
  foldl (flip const) 'z' ['a'..'f']
