module Knyaz.File (
  maybeReadFile,
  maybeReadLines
  ) where

import Control.Monad

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile path =
  catch (liftM Just $ readFile path)
        (\_ -> return Nothing)

maybeReadLines :: FilePath -> IO (Maybe [String])
maybeReadLines path = do
  maybeContents <- maybeReadFile path
  return $ case maybeContents of
    Just contents ->
      Just $ lines contents
    Nothing ->
      Nothing