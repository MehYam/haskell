module Dirr (dirRecursive) where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

dirRecursive :: FilePath -> IO [FilePath]

dirRecursive parent = do
	rawChildren <- getDirectoryContents parent
	let children = filter (`notElem` [".", ".."]) rawChildren
	allChildren <- forM children $ \child -> do
		let childPath = parent </> child
		isDir <- doesDirectoryExist childPath
		if isDir
			then dirRecursive childPath
			else return [childPath]
	return (concat allChildren)
