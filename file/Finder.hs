import Dirr

finder :: (FilePath -> Bool) -> FilePath -> IO [FilePath]

finder p parent = do
	contents <- dirRecursive parent
	return (filter p contents) 