import Data.Char
import Data.Bits
import Numeric

{-

Some code that performs the building blocks of OTP

-}

testm = "this is a test message"
testk = "foodadoosdfsdfsdfsdfsd"

ordsm = stringToOrds testm
ordsk = stringToOrds testk

-- xor two lists of bytes, can be used for OTP
crypt :: [Int] -> [Int] -> [Int]
crypt m k = zipWith xor m k

-- convert string to byte list
stringToOrds :: String -> [Int]
stringToOrds = map ord

-- convert byte list to string
ordsToString :: [Int] -> String
ordsToString = map chr 

-- convert hex string to byte list
hexToOrds :: String -> [Int]
hexToOrds [] = []
hexToOrds (x:y:xs) = digit : hexToOrds xs	
	where ((digit, _):_) = readHex([x, y])

