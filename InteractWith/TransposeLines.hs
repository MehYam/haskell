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
--myFunction input = transposeLines $ lines input
myFunction input = foo $ lines input

foo :: [[a]] -> [a]
foo [] = []
foo ([]:ys) = [] : foo ys
foo ([x:xs]:ys) = [x] : foo ys

--transposeLines :: [[a]] -> [[a]]
