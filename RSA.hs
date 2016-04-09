{-# LANGUAGE BangPatterns #-}
module RSA where

import Math

data RSA =
  RSA {
    p :: Z,
    q :: Z,
    n :: Z,
    tot :: Z,
    e :: Z,
    d :: Z
  } deriving (Show)

data Pub  = Pub  Z Z deriving (Show, Read)
data Priv = Priv Z Z deriving (Show, Read)

twoLargeRandomPrimes :: Integer -> IO (Z,Z)
twoLargeRandomPrimes size = do
  i <- randomLargePrime size
  j <- randomLargePrime (size - 4)
  return (i, j)

generateRSA :: Integer -> IO (RSA, Pub, Priv)
generateRSA size = do
  (p, q) <- twoLargeRandomPrimes size
  let n = p * q
  let tot = (p-1) * (q-1)
  let e = 65537
  let Just d = inverse e tot
  return (RSA p q n tot e d, Pub n e, Priv n d)

encrypt :: Pub -> String -> String
encrypt (Pub n e) s = decode (powm (encode s) e n)

decrypt :: Priv -> String -> String
decrypt (Priv n d) s = decode (powm (encode s) d n)

sign :: Priv -> String -> (String, String)
sign priv message = (message, decrypt priv (hash message)) where

data Verification a = Approved a | InvalidSignature deriving (Show)

verify :: Pub -> (String, String) -> Verification String
verify pub (message, signature) =
  if hash message == encrypt pub signature
    then Approved message
    else InvalidSignature

type CipherText = String

crackText :: Pub -> CipherText -> String
crackText pub c = f 0 where
  f i = if c == encrypt pub (decode i) then decode i else f (i+1)

crackKey :: Pub -> Priv
crackKey (Pub n e) =
  let [_, p, q, _] = factors n in
  let Just d = inverse e ((p-1)*(q-1)) in
  Priv n d

crackKey' :: Pub -> Priv
crackKey' (Pub n e) =
  let t = totient n in
  let Just d = inverse e t in
  Priv n d
