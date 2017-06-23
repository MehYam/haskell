--Exercise 1
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

toDigits :: Integer -> [Integer]
toDigits n = myReverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0 = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

--Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x:[]) = [x]
doubleEveryOther (x:y:zs) = x : (y*2) : doubleEveryOther zs

--Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | x < 10 = x
  | otherwise = sumDigits (toDigits x)
sumDigits (x:xs) = sumDigits (toDigits x) + sumDigits xs

--Exercise 4
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigitsRev n) ) `mod` 10) == 0

--Exercise 5
type Peg = Integer
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
