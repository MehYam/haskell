import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- basics, declaring things ---------------------
doubleMe x = x + x
doubleTwo x y = doubleMe x + doubleMe y
doubleMaybe x = if x < 5
	then doubleMe x
	else succ x
removeUpper str = [c | c <- str, c `elem` ['a'..'z']]
chars = ['#'..'z']
length2 xs = sum [1 | _ <- xs]

-- types and typeclasses -------------------------
-- factorial n = product [1..n]
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

fib :: (Integral a) => a -> a
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)

circ :: Floating a => a -> a
circ r = 2 * pi * r
circ' :: Double -> Double
circ' r = 2 * pi * r

-- pattern matching ---------------------------------
lucky :: (Integral a, Show a) => a -> String
lucky 7 = "lucky seven!"
lucky x = "not seven: " ++ (show x)

addPoints :: (Num a) => (a, a) -> (a, a) -> (a, a)
addPoints (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- pattern matching with mys ---------------------
head' :: [a] -> a
head' [] = error "list is empty"
head' (x:_) = x  -- remember that parens here don't mean a tuple.  See http://learnyouahaskell.com/syntax-in-functions

describe :: Show a => [a] -> String
describe [] = "That's the empty list"
describe (x:[]) = "List with one element: " ++ show x
describe (x:y:[]) = "List with two elements: " ++ show x ++ ", " ++ show y
describe (x:y:_) = "List with many elements"

length3 :: Num b => [a] -> b
length3 [] = 0
length3 (_:x) = 1 + length3 (x)

sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (x:xs) = x + sum2 xs

judge weight height
        | bmi < light = "go eat something"
        | bmi < heavy = "normal"
        | bmi < absurd = "put down the cheeseburger"
        | otherwise  = "put down the cow"
        where
        	bmi = weight / height ^ 2
        	(light, heavy, absurd) = (18.5, 25.0, 30.0)

initials firstname lastname = [f] ++ "." ++ " " ++ [l] ++ "."
	where
		(f:_) = firstname
		(l:_) = lastname

max2 a b
	| a < b = b
	| otherwise = a

compare2 :: Ord a => a -> a -> Ordering
compare2 a b
	| a < b = LT
	| a == b = EQ
	| otherwise = GT

sample = [x `mod` 70 | x <- [100,499..10000]]

myMax :: Ord a => [a] -> a
myMax [] = error "can't myMax an empty my"
myMax [x] = x
myMax (x:xs) = max x (myMax xs)

-- my attempt (less correct, doesn't handle negatives)
myFill 0 item = []
myFill count item = item:(myFill (count-1) item)

-- fix
myFill2 :: (Num a, Ord a) => a -> a1 -> [a1]
myFill2 count item
	| count <= 0 = []
	| otherwise = item:(myFill2 (count-1) item)


myTake 0 x = []
myTake count [] = []
myTake count (x:xs) = x:(myTake (count-1) xs)

myTake2 count list
	| count <= 0 = []
	| list == [] = []
	| otherwise = x:(myTake2 (count-1) xs)
	where (x:xs) = list

myTake3 count _
	| count <= 0 = []        -- that's a new one... the clauses can cause a pattern match with the rest of the patterns
myTake3 _ [] = []
myTake3 count (x:xs) = x:(myTake3 (count-1) xs)


myRev [] = []
myRev (x:xs) = (myRev xs) ++ [x]

myRepeat x = x:(myRepeat x)  -- this language is amazing

myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):(myZip xs ys)

myContains _ [] = False
myContains el (x:xs)
	| el == x = True
	| otherwise = myContains el xs


mySort [] = []
mySort (x:xs) = []

-- higher-order functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

collatz 1 = [1]
collatz n
	| even n = n:collatz(n `div` 2)
	| otherwise = n:collatz(n * 3 + 1)

-- implementing library functions with folds
-- myMaximum xs = foldl (\acc x -> max acc x) 0 xs	
-- myMaximum [] = error "empty list"
-- myMaximum (x:xs) = foldl (max) x xs	-- UNNECESSARY - can use foldl1 to use first list item as initial accumulator
-- myMaximum xs = foldl1 (max) xs       -- UNNECESSARY - don't need to pass xs around! ------------------------------
myMaximum :: (Ord a) => [a] -> a
myMaximum = foldl1 (max)

myRev2 = foldl (\acc x -> x:acc) []
myRev3 = foldl (flip (:)) []

myFilter f = foldr (\x acc -> if f x then x:acc else acc) []

-- function application

-- "How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?"
foo = length (takeWhile (<1000) (scanl1 (\acc x -> (sqrt x) + acc) [1..]))

-- $ has the lowest precedence of any operator
foo2 = length $ takeWhile (<1000) $ scanl1 (\acc x -> (sqrt x) + acc) [1..]

-- 




-- data types, value constructors
data Bike = MTB | Cyclocross | Hybrid | Road deriving (Eq, Ord, Read, Bounded, Enum)
data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs (x2 - x1)) * (abs (y2 - y1))

data Person = Person	{firstName :: String
						,lastName :: String
						,age :: String
						,height :: String
						,phoneNumber :: String
						,flavor :: String
						} deriving (Show, Eq)


data Vector a = Vector a a a deriving (Eq, Ord, Read, Show)

vecAdd :: (Num a) => Vector a -> Vector a -> Vector a
vecAdd (Vector v1 v2 v3) (Vector w1 w2 w3) = Vector (v1 + w1) (v2 + w2) (v3 + w3)


data LockerState = Taken | Free deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

-- Algebraic data types
infixr 5 :+
data List a = EmptyList | a :+ (List a) deriving (Eq, Ord, Show, Read)

infixr 5 .++
(.++) :: List a -> List a -> List a
EmptyList .++ xs = xs
(x :+ xs) .++ ys = x :+ (xs .++ ys) -- "Notice how we pattern matched on (x :+ xs). That works because pattern matching is actually about matching constructors."

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)

treeRoot :: a -> Tree a
treeRoot x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = treeRoot x
treeInsert x (Node a left right)
	| x < a = Node a (treeInsert x left) right
	| x > a = Node a left (treeInsert x right) 
	| otherwise = Node x left right

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
	| x < a = treeElem x left
	| x > a = treeElem x right
	| otherwise = True

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList xs = foldr treeInsert EmptyTree xs

sampleTree = treeFromList sample

-- typeclasses 102 ------------------------------------------------------------------

instance Show Bike where
	show MTB = "mountain bike"
	show Cyclocross = "cyclocross"
	show Hybrid = "worst of both worlds"
	show Road = "bike for tight lycra wearers"

class JavascriptBool a where
	jsbool :: a -> Bool

instance JavascriptBool Int where
	jsbool 0 = False
	jsbool _ = True

instance JavascriptBool [a] where
	jsbool [] = False
	jsbool _ = True

instance JavascriptBool Bool where
	jsbool = id

instance JavascriptBool (Maybe a) where
	jsbool (Just _) = True
	jsbool Nothing = False

instance JavascriptBool (Tree a) where
	jsbool EmptyTree = False
	jsbool _ = True

instance JavascriptBool Bike where
	jsbool MTB = True
	jsbool _ = False

ifJSB :: (JavascriptBool b) => b -> a -> a -> a
ifJSB jsb trueResult falseResult = if jsbool jsb then trueResult else falseResult

-- the Functor typeclass -------------------------------------------

instance Functor Tree where
	fmap f EmptyTree = EmptyTree
	fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

