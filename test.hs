import Data.List (sortBy, foldl')
import Data.Char (digitToInt)

--sample = [fromInteger (x `mod` 70) :: Double | x <- [100,499..10000]]
sample = [x `mod` 70 | x <- [100,499..10000]]


myLen :: [a] -> Integer
myLen (_:xs) = 1 + myLen xs
myLen _ = 0

myMean :: (Integral a, Fractional b) => [a] -> b
myMean xs = myMeanInternal 0 0 xs
	where myMeanInternal acc count [] = fromIntegral acc / count
	      myMeanInternal acc count (x:xs) = myMeanInternal (acc + x) (count + 1) xs


myPal :: [a] -> [a]
myPal xs = xs ++ myRev xs
	where myRev [] = []
	      myRev (x:xs) = myRev xs ++ [x]

isPal :: (Eq a) => [a] -> Bool
isPal xs = xs == reverse xs

sortLists :: [[a]] -> [[a]]
sortLists xs = sortBy lengthCompare xs
	where lengthCompare xs ys = compare (length xs) (length ys)

myJoin :: a -> [[a]] -> [a]
myJoin delim [] = []
myJoin delim (xs:[]) = xs
myJoin delim (xs:ys) = xs ++ [delim] ++ (myJoin delim ys)


data Tree a = Node a (Tree a) (Tree a) | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" (Node "left leaf" Empty Empty) (Node "right leaf" Empty Empty))

treeHeight :: Tree a -> Integer
treeHeight Empty = 0
treeHeight (Node a left right) = 1 + max (treeHeight left) (treeHeight right)

data Direction = DirLeft | DirStraight | DirRight deriving (Read, Show, Eq, Ord)
data Point a = Point a a deriving (Show, Eq)


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:xs) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (x:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit xs = Just (init xs)


mySplitWith :: (a -> Bool) -> [a] -> [[a]]
mySplitWith _ [] = []
mySplitWith f xs = front : mySplitWith f (dropWhile f back)
	where (front, back) = break f xs

-- a list of the first item of each list
myTranspose :: [[a]] -> [[a]]
myTranspose [] = []
myTranspose xss
	| allEmpty xss = []
	| otherwise = listOfHeads xss : myTranspose (listOfTails xss)

listOfHeads :: [[a]] -> [a]
listOfHeads [] = []
listOfHeads ([]:xss) = listOfHeads xss
listOfHeads ((x:xs):xss) = x : listOfHeads xss

listOfTails :: [[a]] -> [[a]]
listOfTails [] = []
listOfTails ([]:xss) = [] : listOfTails xss
listOfTails ((x:xs):xss) = xs : listOfTails xss

allEmpty :: [[a]] -> Bool
allEmpty [] = True
allEmpty ((x:xs):xss) = False
allEmpty ([]:xss) = allEmpty xss

----------------------------------------------------
mySum :: Num a => [a] -> a
mySum = foldl (+) 0

myToInt :: String -> Int
myToInt [] = 0
myToInt ('-':xs) = (-1) * (myToInt xs)
myToInt xs = foldl' (\acc x -> acc*10 + (digitToInt x)) 0 xs

myConcat :: [[a]] -> [a]
myConcat = foldr (++) []

-- folds

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f (x:xs)
	| f x = x : (takeWhile1 f xs)
	| otherwise = []

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f = foldr (\x acc -> if f x then x:acc else []) []

-- KAI: left off here.  takeWhile2 works, and I think it's absolutely equivalent to takeWhile1 in the steps its doing, but trace them out to understand why.
-- This is crucial to understanding how foldr and foldl work, and the true difference between them

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ z [] = z
foldr2 f z (x:xs) = f x (foldr2 f z xs)


takeWhile3 :: (a -> Bool) -> [a] -> [a]
takeWhile3 f = foldr2 (\x acc -> if f x then x:acc else []) []

takeWhile4 :: (a -> Bool) -> [a] -> [a] -- same as takeWhile2, it just doesn't use the lambda as we're not supposed to know about those yet
takeWhile4 f = foldr step []
	where step x acc
		| f x = x:acc
		| otherwise = []

groupBy1 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy1 f = foldr step []
	where step x [] = [[x]]
	      step x ((y:ys):yss) = if f x y then ((x:y:ys):yss) else ([x]:(y:ys):yss)

-- experiment - inefficient because it reverses the list, but still interesting
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 f = reverse.foldl step []
	where step [] x = [[x]]
	      step ((y:ys):yss) x = if f x y then ((x:y:ys):yss) else ([x]:(y:ys):yss)

any1 :: (a -> Bool) -> [a] -> Bool
any1 f = foldr (\x acc -> if f x then True else acc) False

cycle1 :: [a] -> [a]
cycle1 xs = xs ++ (cycle1 xs)

-- WRONG - I can't think of a way to do this with a fold that isn't just a replication of cycle1
cycle2 :: [a] -> [a]
cycle2 xs = foldr (\x acc -> x:acc) xs xs 

-- WRONG - it fails with leading spaces
words1 :: String -> [String]
words1 = foldr step [] where
	step x [] = if x == ' ' then [] else [[x]]
	step x ([]:yss) = if x == ' ' then ([]:yss) else ([x]:yss)
	step x ((y:ys):yss) = if x == ' ' then ([]:(y:ys):yss) else ((x:y:ys):yss)


unlines1 :: [String] -> String
unlines1 = foldr step [] where
	step x acc = x ++ "\n" ++ acc


---- typeclasses

class MyEq a where
	isEqual :: a -> a -> Bool

instance MyEq Bool where
	isEqual True True = True
	isEqual False False = True
	isEqual _ _ = False	

class MyEq2 a where -- shows how you can provide actual implementations in the typeclass definition
	isEqual2 :: a -> a -> Bool
	isEqual2 a b = not $ notEqual2 a b

	notEqual2 :: a -> a -> Bool
	notEqual2 a b = not $ isEqual2 a b

instance MyEq2 Bool where
	isEqual2 True True = True
	isEqual2 False False = True
	isEqual2 _ _ = False	



