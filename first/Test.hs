import FileTools
import System.FilePath

files = FileTools.dirR "c:/source/haskell"

foo :: [FilePath] -> [FilePath]
foo = filter (\p -> takeExtension p `elem` [".hs", ".hi"])

-- type "files", then foo it