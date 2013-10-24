import  Data.List

sample = [x `mod` 70 | x <- [100,499..10000]]

numUnique :: (Eq a) => [a] -> Int
numUnique = length . nub