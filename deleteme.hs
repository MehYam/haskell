import Data.List 
import qualified Data.Map as Map
import qualified Data.Set as Set

sample = [x `mod` 70 | x <- [100,499..10000]]
fox = "the quick brown fox jumps over the lazy dog"

myMax :: (Ord a) => [a] -> a
myMax [] = error "empty list has no max"
myMax [x] = x
myMax (x:xs)
	| x > maxTail = x
	| otherwise = maxTail
	where maxTail = myMax xs

myMax2 :: (Ord a) => [a] -> a
myMax2 [] = error "empty list has no max"
myMax2 [x] = x
myMax2 (x:xs) = max x (myMax2 xs)

myRepl :: Int -> b -> [b]
myRepl n e
	| n <= 0 = []
	| otherwise = e:(myRepl (n-1) e)

myRepl2 :: Int -> b -> [b]
myRepl2 0 _ = [] --------- less correct than the above
myRepl2 n e = e:(myRepl (n-1) e)

myTake :: Int -> [a] -> [a]
myTake n (x:xs)
	| n <= 0 = []
	| otherwise = x:(myTake (n-1) xs)

myTake2 :: Int -> [a] -> [a]
myTake2 0 _ = [] --------- less correct than the above
myTake2 n (x:xs) = x:(myTake (n-1) xs)

myRev2 :: [a] -> [a]
myRev2 [] = []
myRev2 (x:xs) = myRev2 xs ++ [x]

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : (myZip xs ys)

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs)
	| e == x = True
	| otherwise = myElem e xs

mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort (x:xs) = 
	let	lesser = [a | a <- xs, a <= x]
		greater = [a | a <- xs, a > x]
	in (mySort lesser) ++ [x] ++ (mySort greater)
	
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys

myFlip :: (a -> b -> c) -> (b -> a -> c)
myFlip f = g where g x y = f y x

-- this is the same as the above, really, but this works because functions are curried by default
myFlip2 :: (a -> b -> c) -> (b -> a -> c)
myFlip2 f x y = f y x

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
--myFilter f (x:xs) = if (f x) then x:(myFilter f xs) else (myFilter f xs)
myFilter f (x:xs)
	| f x = x : myFilter f xs
	| otherwise = myFilter f xs

mySort2 :: Ord a => [a] -> [a]
mySort2 [] = []
mySort2 (x:xs) = mySort2 (filter (<= x) xs) ++ [x] ++ mySort2 (filter (> x) xs)

result limit = maximum [x | x <- [0..limit], (x `mod` 3829) == 0]

result2 factor limit
	| (factor > limit) = error "factor is greater than limit"
	| otherwise = result2b factor factor limit
result2b factor currentTotal limit 
	| (newTotal > limit) = currentTotal
	| otherwise = result2b factor newTotal limit
	where newTotal = factor + currentTotal

result3 = [x^2 | x <- [0..], odd $ x^2]

-- folds
mySum :: (Num a) => [a] -> a
mySum = foldl (+) 0

myElem2 :: (Eq  a) => a -> [a] -> Bool
--myElem2 e list = foldl (\acc x -> acc || (x == e)) False list
myElem2 e = foldl (\acc x -> acc || (x == e)) False

myMap2 :: (a -> b) -> [a] -> [b]
myMap2 f = foldr (\x acc -> (f x):acc) []

myMax3 :: (Ord a) => [a] -> a
myMax3 = foldl1 max

myRev3 :: [a] -> [a]
myRev3 = foldl (\acc x -> x:acc) []

myProduct :: (Num a) => [a] -> a
myProduct = foldl1 (*)

myFilter2 :: (Eq a) => (a -> Bool) -> [a] -> [a]
myFilter2 f = foldr (\x acc -> if (f x) then x:acc else acc) []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile f (x:xs) = if (f x) then x:(myTakeWhile f xs) else []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ [] = []
myDropWhile f list@(x:xs) = if (f x) then myDropWhile f xs else list

inv = 
	[("bike", 345.99)
	,("guitar", 600)
	,("car", 10000)
	,("ice cream", 5.99)
	,("TV", 999.99)
	]

invMap = Map.fromList inv

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k then Just v else findKey key xs


