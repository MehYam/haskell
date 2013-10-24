import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
	args <- getArgs
	case args of
		[input,output] -> interactWith function input output
		_ -> putStrLn "error: exactly two arguments needed"

-- replace "id" with the name of our function below
myFunction content = firstWord $ lines content

safeHead [] = []
safeHead (x:xs) = x

firstWord [] = []
firstWord (thisLine:remainingLines) = (safeHead $ words thisLine) ++ "\n" ++ (firstWord remainingLines)