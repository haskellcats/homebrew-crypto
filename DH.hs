{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module DH where

import Math
import System.Random
import Control.Spoon
import Data.Maybe
import Data.Function

{-
load DH params with loadDH
use dhDialog to actually do an exchange
-}

-- the distillation of the two step process
simpleDH1 :: Z -> Z -> Z -> Z
simpleDH1 p g a = powm g a p

simpleDH2 :: Z -> Z -> Z -> Z
simpleDH2 p a bob = powm bob a p

-- a set of types that forces you to follow the DH workflow
data Start = Start
data Ready = Ready
data NeedHandshake = NeedHandshake
data Complete = Complete
data KeepSecret a = KeepSecret a deriving (Read, Show)

data DH :: * -> * where
  DHStart :: DH Start
  DHReady :: DH Start -> Z -> Z -> DH Ready
  DHNeedHandshake :: DH Ready -> KeepSecret Z -> Z -> DH NeedHandshake
  DHComplete :: DH NeedHandshake -> KeepSecret Z -> DH Complete

deriving instance Show (DH a)

loadDH :: String -> DH Ready
loadDH x = let (p,g) = read x in DHReady DHStart p g

saveDH :: String -> DH Ready -> IO ()
saveDH filePath (DHReady _ p g) = writeFile filePath (show (p,g))

getPublicPart :: DH NeedHandshake -> Z
getPublicPart (DHNeedHandshake _ _ z) = z

getSharedSecret :: DH Complete -> Z
getSharedSecret (DHComplete _ (KeepSecret z)) = z

generateGroup :: DH Start -> Integer -> IO (DH Ready)
generateGroup dh size = do
  p <- randomLargePrime size
  return $ DHReady dh p 2

begin :: DH Ready -> IO (DH NeedHandshake)
begin dh@(DHReady _ p g) = do
  a <- randomRIO (2, p-1)
  return $ DHNeedHandshake dh (KeepSecret a) (powm g a p)

complete :: DH NeedHandshake -> Z -> DH Complete
complete dh@(DHNeedHandshake (DHReady _ p _) (KeepSecret a) _) bobsPart =
  DHComplete dh (KeepSecret (powm bobsPart a p))

dhDialog :: DH Ready -> IO (Z, Z -> Z)
dhDialog dh = do
  dh' <- begin dh
  return (getPublicPart dh', \otherPart -> getSharedSecret (complete dh' otherPart))




-- a classical method of keeping track of the DH parameters
-- (everything below this point is joking)
data DHClass = DHClass
  { dhP :: Z
  , dhG :: Z
  , dhA :: Z
  , dhS :: Z
  , dhBegin :: IO (Z, DHClass)
  , dhComplete :: Z -> DHClass }

instance Show DHClass where
  show (DHClass p g a s begin complete) = concat
    ["DHClass { "
    ,"dhP = ", show p, ", "
    ,"dhG = ", show g, ", "
    ,"dhA = ", fromMaybe "_|_" (show <$> teaspoon a), ", "
    ,"dhS = ", fromMaybe "_|_" (show <$> teaspoon s), ", "
    ,"dhBegin = ", haha "_|_" "<ACTION>" begin, ", "
    ,"dhComplete = ", haha "_|_" "<Z -> ACTION>" complete, "}"]

haha :: String -> String -> a -> String
haha ok bad x = case teaspoon x of
  Nothing -> ok 
  Just _ -> bad

newClassicDH :: Integer -> IO DHClass
newClassicDH size = do
  p <- randomLargePrime size
  let g = 2
  return $ fix $ \this -> do
    let begin = do
          a <- randomRIO (2, p-1)
          return (powm g a p, this { dhA = a, dhBegin = undefined })
    let complete bob = this { dhS = powm bob (dhA this) (dhP this), dhComplete = undefined }
    DHClass p g undefined undefined begin complete

