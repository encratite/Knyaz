module Knyaz.Directory(
                      FileInformation(..),
                      readDirectory
                      ) where

import Data.List
import System.Directory
import System.FilePath

data FileInformation = FileInformation {
  fileName :: FilePath,
  filePath :: FilePath,
  fileIsDirectory :: Bool
  }

-- | Try to read the contents of a directory.
-- The results are sorted by their name.
readDirectory :: FilePath -> IO (Maybe [FileInformation])
readDirectory directory =
  catch (do names <- getDirectoryContents directory
            let filteredNames = sort $ filter nameFilter names
            contents <- mapM constructInformation filteredNames
            return $ Just contents)
        (\_ -> do return Nothing)
  where
    nameFilter name = not $ elem name [".", ".."]

    constructInformation :: String -> IO FileInformation
    constructInformation name = do
      let path = directory </> name
      isDirectory <- doesDirectoryExist path
      return $ FileInformation name path isDirectory