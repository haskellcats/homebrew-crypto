{-# LANGUAGE OverloadedStrings #-}
module StreamCipher where

import Math
import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Data.Bits
import System.Random

type S = Z
data SS = SS BS.ByteString S deriving (Show, Read)

-- heh...
csrng :: IO Integer
csrng = randomRIO (2^127, 2^128 - 1)

newStreamCipher :: IO Integer -> IO SS
newStreamCipher csrng = do
  i <- csrng
  return (SS BS.empty i)

initializeStreamCipher :: S -> SS
initializeStreamCipher i = SS "" i

crypt :: SS -> BS.ByteString -> (BS.ByteString, SS)
crypt ss@(SS keyChunk i) textChunk =
  let kL = BS.length keyChunk in
  let tL = BS.length textChunk in
  if kL < tL
    then crypt (moreKeyStream ss) textChunk
    else
      let (pad, keyChunk') = BS.splitAt tL keyChunk in
      let textChunk' = BS.pack $ BS.zipWith xor pad textChunk in
      (textChunk', SS keyChunk' i)

moreKeyStream :: SS -> SS
moreKeyStream (SS keyChunk i) = SS (keyChunk <> more) (i + 1) where
  more = sha512 (show i)
