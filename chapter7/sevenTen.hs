module SevenTen where
print' :: Show a => a -> IO ()
-- print a = putStrLn (show a)
-- or
print' = putStrLn . show