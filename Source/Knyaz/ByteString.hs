module Knyaz.ByteString(
  findSubByteString
  ) where

import Data.ByteString as DB

findSubByteString :: DB.ByteString -> DB.ByteString -> Maybe Int
findSubByteString string target =
  if DB.null target
  then Just 0
  else case DB.breakSubstring target string of
    (brokenHead, brokenTail) | DB.null brokenTail -> Nothing
                             | otherwise -> Just $ DB.length brokenHead
