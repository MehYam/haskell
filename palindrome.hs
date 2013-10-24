import Data.Char

main = interact $ (map toUpper) . unlines . map (\x -> if x == reverse x then "palindrome" else "not palindrome") . lines