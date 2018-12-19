module ZippingExercises where
-- 1.
zip' :: [a] -> [b] -> [(a, b)]
zip' a b = zipWith' (,) a b

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs