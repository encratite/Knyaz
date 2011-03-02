module Knyaz.File (
  maybeReadFile,
  maybeReadLines
  ) where

import Control.Monad
import qualified Data.ByteString as DB
import Data.Char
import Data.Word
import Data.Vector as DV

maybeReadFile :: FilePath -> IO (Maybe DB.ByteString)
maybeReadFile path =
  catch (liftM Just $ DB.readFile path)
        (\_ -> return Nothing)

maybeReadLines :: FilePath -> IO (Maybe (DV.Vector DB.ByteString))
maybeReadLines path = do
  maybeContents <- maybeReadFile path
  return $ case maybeContents of
    Just contents ->
      let newline = (fromIntegral $ ord '\n') :: Word8 in
      Just . DV.fromList $ DB.split newline contents
    Nothing ->
      Nothing