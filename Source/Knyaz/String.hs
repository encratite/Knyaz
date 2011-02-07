module Knyaz.String(
  readMaybe,
  leftTrim,
  rightTrim,
  trimString,
  tokeniseByteString,
  tokeniseString
  ) where

import Data.Char
import qualified Data.ByteString as DB
import qualified Data.ByteString.UTF8 as DBU

-- | Attempt to convert an entire string to the desired type which is an instance of Read.
readMaybe :: Read a => String -> Maybe a
readMaybe string =
  let results = reads string in
  if null results
  then
    -- failed to parse anything
    Nothing
  else
    -- managed to parse at least one object
    let readResult = head results
        object = fst readResult
        remainingString = snd readResult
    in
     if null remainingString
     then
       -- successfully parsed the entire string
       Just object
     else
       -- the string was not processed fully
       Nothing

-- | Trim the whitespace off the left side of a string.
leftTrim :: String -> String
leftTrim = dropWhile isSpace

-- | Trim the whitespace off the right side of a string.
rightTrim :: String -> String
rightTrim = reverse . leftTrim . reverse

-- | Trim the whitespace off both the left and the right side of a string.
trimString :: String -> String
trimString = leftTrim . rightTrim

-- | Tokenise a bytestring.
tokeniseByteString :: DB.ByteString -> DB.ByteString -> [DB.ByteString]
tokeniseByteString string delimiter =
  collected : if DB.null remaining
              then []
              else otherTokens
  where
    (collected, remaining) = DB.breakSubstring delimiter string
    otherTokens =
      let withoutDelimiter = DB.drop (DB.length delimiter) remaining in
      tokeniseByteString withoutDelimiter delimiter

-- | Tokenise a string.
tokeniseString :: String -> String -> [String]
tokeniseString string delimiter =
  map DBU.toString $ tokeniseByteString (byteString string) (byteString delimiter)
  where
    byteString = DBU.fromString