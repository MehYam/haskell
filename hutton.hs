-- work from the "Programming in Haskell" book.  Learning Haskell for the third time!

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
