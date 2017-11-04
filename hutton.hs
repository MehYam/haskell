-- work from the "Programming in Haskell" book.  Learning Haskell for the third time!

foo = [0..10]

--------------------------------
-- chapter 1 -------------------
--3--
product_13 :: (Num t) => [t] -> t
product_13 [] = 1;
product_13 (x:xs) = x * product_13 xs

-- myProduct [2,3,4]
--    = 2 * myProduct [3, 4]
--    = 2 * (3 * myProduct [4])
--    = 2 * (3 * (4 * myProduct []))
--    = 2 * (3 * (4 * 1)) == 2 * 3 * 4 = 24

--4--
qsort_14 :: (Ord a) => [a] -> [a]
qsort_14 [] = []
qsort_14 (x:xs) = qsort_14 larger ++ [x] ++ qsort_14 smaller
   where
      smaller = [a | a <- xs, a <= x]
      larger = [b | b <- xs, b > x]

--5-- '<' instead of '<='' would just remove duplicates

--------------------------------
-- chapter 2 -------------------
--2--
--    (2^3)*4
--    (2*3) + (4*5)
--    2 + 3*(4^5)

--3--
n_23 = a `div` length xs
   where
      a = 10
      xs = [1,2,3,4,5]

--4--
last_241 xs = head $ reverse xs
last_242 xs = xs !! (length xs - 1) -- doesn't handle empty list, but neither does 'last'
last_243 [x] = x
last_243 (x:xs) = last_243 xs

--5--
init_251 xs = take (length xs - 1) xs
init_252 [x] = []
init_252 (x:xs) = x:(init_252 xs)

--------------------------------
-- chapter 3 -------------------
--2--
{-
bools :: [Bool]
bools = [False, True]

nums :: [[Int]]
nums = [[1], [2], [3]]

copy :: a -> (a, a)
copy a = (a, a)

apply :: (a -> b) -> a -> b
apply f a = f a
-}

--------------------------------
-- chapter 4 -------------------

--1--
halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (take (half xs) xs, drop (half xs) xs)
   where half xs = length xs `div` 2

--2--
--third :: [a] -> a
third0 xs = head (tail (tail xs))
third1 xs = xs !! 2
third2 (a:b:c:xs) = c

--3--
listIsEmpty :: [a] -> Bool
listIsEmpty [] = True
listIsEmpty _ = False

safeTail1 :: [a] -> [a]
safeTail1 xs
   | listIsEmpty xs = []
   | otherwise = tail xs

safeTail2 :: [a] -> [a]
safeTail2 xs = if (listIsEmpty xs) then [] else tail xs

safeTail3 :: [a] -> [a]
safeTail3 [] = []
safeTail3 xs = tail xs

--4--
(@@) :: Bool -> Bool -> Bool
-- first ------------------:
-- True @@ True = True
-- True @@ False = True
-- False @@ True = True
-- False @@ False = False
-- second------------------:
-- False @@ False = False
-- _ @@ _ = True
-- third------------------:
-- False @@ b = b
-- True @@ _ = True
-- fourth -----------------:
a @@ b | a == b = a
       | otherwise = True

--8--
luhnDouble :: Int -> Int
luhnDouble a = luhnBound (a*2) where
   luhnBound b | b > 9 = b - 9
               | otherwise = b

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0


