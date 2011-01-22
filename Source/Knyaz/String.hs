module Knyaz.String(
  readMaybe
  ) where

-- | Attempt to convert an entire string to the desired type which is an instance of Read.
readMaybe :: Read a => String -> Maybe a
readMaybe string =
  let results = reads string in
      if null results then
        -- failed to parse anything
        Nothing
      else
        -- managed to parse at least one object
        let readResult = head results
            object = fst readResult
            remainingString = snd readResult
            in
         if null remainingString then
           -- successfully parsed the entire string
           Just object
         else
           -- the string was not processed fully
           Nothing
