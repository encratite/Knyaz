module Knyaz.Directory(
                      FileInformation,
                      fileName,
                      filePath,
                      readDirectory
                      ) where

import Data.List
import System.Directory
import System.FilePath

data FileInformation = FileInformation {
  fileName :: FilePath,
  filePath :: FilePath
  }

readDirectory :: FilePath -> IO (Maybe [FileInformation])
readDirectory directory =
  catch (do names <- getDirectoryContents directory
            let filteredNames = sort $ filter nameFilter names
            return . Just $ map constructInformation filteredNames)
        (\_ -> do return Nothing)
  where
    nameFilter name = not $ elem name [".", ".."]
    constructInformation name = FileInformation name $ directory </> name