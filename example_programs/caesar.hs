import Data.Char

{- Caesar's Cipher - shift letters along N places in
   the alphabet to encrypt a message. This is a
   basic subsitution cipher.
-}

bumpCharBy :: Int -> Char -> Char
-- | shift character n letters along the
-- | alphabet - wrap around from Z to A
-- | preserve case of character
-- | leave non-alphabetical chars alone
bumpCharBy n c =
  let
    newCode = (ord c) + (n `mod` 26)
    needToWrap =
      (isUpper c && newCode > ord 'Z') ||
      (isLower c && newCode > ord 'z')
    newCode' = if needToWrap
               then newCode-26
               else newCode
  in 
    if isLetter c
    then
      chr newCode'
    else
      c
  

caesar :: Int -> String -> String
caesar n s = map (bumpCharBy n) s


