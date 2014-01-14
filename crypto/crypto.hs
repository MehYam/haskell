import Data.Char
import Data.Bits
import Numeric

{-

Some code that performs the building blocks of OTP

-}

testm = "this is a test message"
testk = "foodadoosdfsdfsdfsdfsd"

ordsm = toOrds testm
ordsk = toOrds testk

-- do the OTP (encrypt ordsm ordsk)
encrypt :: [Int] -> [Int] -> [Int]
encrypt m k = zipWith xor m k

-- convert string to ords
toOrds :: String -> [Int]
toOrds x = map ord x

-- convert hex string to [Int]
--toHexString :: [Int] -> String
--toHexString [] = []
--toHexString x:y:xs = hexDigit ++

--	readHex $ take 2 xs 
--foo :: String -> [Int]
foo [] = []
foo (x:y:xs) = digit : foo xs	
	where ((digit, _):_) = readHex([x, y])


((digit, _):_) = readHex(['a', 'b'])

-- convert [Int] to string