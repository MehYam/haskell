-- work from the "Programming in Haskell" book.  Learning Haskell for the third time!

--------------------------------
-- chapter 1 exercises ---------
-- 3.
product_1_3 :: (Num t) => [t] -> t
product_1_3 [] = 1;
product_1_3 (x:xs) = x * product_1_3 xs

-- myProduct [2,3,4]
--    = 2 * myProduct [3, 4]
--    = 2 * (3 * myProduct [4])
--    = 2 * (3 * (4 * myProduct []))
--    = 2 * (3 * (4 * 1)) == 2 * 3 * 4 = 24

-- 4.
qsort_1_4 :: (Ord a) => [a] -> [a]
qsort_1_4 [] = []
qsort_1_4 (x:xs) = qsort_1_4 larger ++ [x] ++ qsort_1_4 smaller
   where
      smaller = [a | a <- xs, a <= x]
      larger = [b | b <- xs, b > x]

-- 5. '<' instead of '<='' would just remove duplicates

--------------------------------
-- chapter 2 exercises ---------
