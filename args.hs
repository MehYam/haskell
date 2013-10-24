import System.Environment
import Data.List

main = do
	args <- getArgs
	mapM putStrLn ("args: ":args)

	name <- getProgName
	putStrLn ("name: " ++ name)