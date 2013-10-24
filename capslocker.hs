import Data.Char  
  
main = do  
    contents <- getContents
    putStrLn "here"
    putStrLn $ map toUpper contents