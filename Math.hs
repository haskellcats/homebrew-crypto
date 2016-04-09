{-# LANGUAGE BangPatterns #-}
module Math where

import Control.Monad
import System.Random
import qualified Data.ByteString as BS
import Data.Char
import Data.Bits
import qualified Data.Set as S
import qualified Math.NumberTheory.Primes.Factorisation as NumberTheory (divisors, totient)
import qualified MillerRabin (isPrime)
import qualified Crypto.Hash.SHA512 as SHA512
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.ByteString.Base16 as Hex

type Z = Integer

-- modular exponentiation b^e mod m
powm :: Z -> Z -> Z -> Z
powm b e m = f b e m 1 where
  f b 0 m r = r
  f b e m r
    | e `mod` 2 == 1 = f (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
    | otherwise      = f (b * b `mod` m) (e `div` 2) m r

-- for any a and b there exists x y and g such that ax+by=g
-- where g = gcd a b
euclid :: Z -> Z -> (Z, Z, Z)
euclid a 0 = (1, 0, a)
euclid a b =
  let (q, r) = a `quotRem` b in
  let (s, t, g) = euclid b r in
  (t, s - q * t, g)

-- compute x such that ax = 1 mod m, if any
inverse :: Z -> Z -> Maybe Z
inverse a m = 
  let (i, _, g) = euclid a m in
  if g == 1
    then Just (if i < 0 then i + m else i)
    else Nothing

-- crt m n a b   finds x (mod m*n) such that x = a (mod m) and x = b (mod n)
-- m and n must be coprime
crt :: Z -> Z -> Z -> Z -> Z
crt m n = case inverse m n of
  Nothing  -> error "crt: moduli are not coprime"
  Just n'  -> \a b -> let t = (a - b) * n' `mod` m
                       in b + n * t 

factors :: Z -> [Z]
factors i = S.toList (NumberTheory.divisors i)

totient :: Z -> Z
totient i = NumberTheory.totient i

encode :: String -> Z
encode [] = 0
encode (c:cs) = (256 * encode cs) + fromIntegral (ord c)

decode :: Z -> String
decode 0 = ""
decode n = let (q,r) = divMod n 256 in chr (fromIntegral r) : decode q

isPrime = MillerRabin.isPrime

makePrime :: Z -> Z
makePrime i = if isPrime i then i else makePrime (i+2)

randomLargePrime :: Integer -> IO Z
randomLargePrime size = do
  i <- randomRIO (2^(size-1), 2^size)
  let !p = makePrime (if even i then i + 1 else i)
  return p

digitCount :: Z -> Int
digitCount i = f i 0 where
  f 0 c = c
  f i c = f (i `div` 10) (c+1)

bitCount :: Z -> Int
bitCount i = f i 0 where
  f 0 c = c
  f i c = f (i `div` 2) (c+1)

hash :: String -> String
hash =
  T.unpack .
  decodeUtf8 .
  Hex.encode .
  SHA512.hash .
  encodeUtf8 .
  T.pack

sha512 :: String -> BS.ByteString
sha512 = SHA512.hash . encodeUtf8 . T.pack
