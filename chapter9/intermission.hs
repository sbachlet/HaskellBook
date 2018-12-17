-- 1. NF
one = [1, 2, 3, 4, 5]
-- 2. whnf
-- two = 1 : 2 : 3 : 4 : _
-- 3. whnf
three = enumFromTo 1 10
-- 4. nf
four = length [1, 2, 3, 4, 5]
-- 5. nf
five = sum (enumFromTo 1 10)
-- 6. nf
six = ['a'..'m'] ++ ['n'..'z']
-- 7. whnf
-- seven = (_, 'b')