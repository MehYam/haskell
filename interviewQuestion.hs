answer :: (Ord t, Num t) => [t] -> [[t]]
answer [] = []
answer x = answer_impl x 


denum :: (Ord t, Num t) => [t] -> [[t]]
denum [] = []
denum (x:y:xs)
   | ((x*10 + y) <= 26) = [x*10 + y] : (denum (y:xs)) ++ [x]:[y]:(denum xs)
   | otherwise = [x]:[y]:(denum xs)
denum (x:xs) = [x]:(denum xs)