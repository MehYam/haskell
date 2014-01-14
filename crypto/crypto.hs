import Data.Char
import Data.Bits
import Numeric

-- Helper methods for working with OTP - reading and writing hex, performing xor's on things
-- KAI: these can most likely be rewritten more simply folds or combinations of existing library functions

-- xor two lists of bytes, can be used for OTP
crypt :: [Int] -> [Int] -> [Int]
crypt m k = zipWith xor m k

-- convert string to byte list
stringToOrds :: String -> [Int]
stringToOrds = map ord

-- convert byte list to string - for debugging, won't work for all byte lists obviously
ordsToString :: [Int] -> String
ordsToString = map chr 

-- convert hex string to byte list
hexToOrds :: String -> [Int]
hexToOrds [] = []
hexToOrds (x:y:xs) = digit : hexToOrds xs	
	where ((digit, _):_) = readHex([x, y])

-- convert byte list to hex string
ordsToHex :: [Int] -> String
ordsToHex [] = []
ordsToHex (x:xs) = (showHex x "") ++ ordsToHex xs

-- week 1 Question 7

m = stringToOrds "attack at dawn"
c = hexToOrds "6c73d5240a948c86981bc294814d"
k = crypt m c

m2 = stringToOrds "attack at dusk"
c2 = crypt m2 k