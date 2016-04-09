{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import DH
import StreamCipher
import Network
import qualified Data.ByteString as BS
import Control.Concurrent
import qualified Data.Text as T
import Data.Text.Encoding
import System.IO (hPutStrLn, hGetLine)
import System.Environment
import System.Exit
import Control.Concurrent.Async
import System.IO
import Data.Monoid ((<>))
import Control.Monad
import Control.Exception

main = do
  -- wait for alice to connect to bob, then connect to bob
  (alice,bob) <- getArgs >>= \case 
    [host, oport, "-p", iport] -> do
      server <- listenOn (PortNumber (fromIntegral (read iport)))
      (alice,_,_) <- accept server
      sClose server
      bob <- connectTo host (PortNumber (fromIntegral (read oport)))
      return (alice,bob)
    _ -> do
      hPutStrLn stderr "usage: host oport -p iport"
      exitFailure
  dh <- loadDH <$> readFile "diffie-hellman-params"
  -- acquire the public parts of their DH handshake
  a1 <- async (read <$> hGetLineEve alice bob) :: IO (Async Integer)
  a2 <- async (read <$> hGetLineEve bob alice) :: IO (Async Integer)
  (dhA,dhB) <- waitBoth a1 a2
  putStrLn $ "Eve: alice said " ++ show dhA
  putStrLn $ "Eve: bob said " ++ show dhB
  -- begin the process of getting their shared secret
  futureSecret <- async (return (crackDH dh dhA dhB))
  -- begin logging the raw data and forwarding it as is, never cancel this
  aliceChan <- newChan
  bobChan <- newChan
  aliceSideChan <- dupChan aliceChan
  bobSideChan <- dupChan bobChan
  sideChan <- newChan
  t1 <- async (tap alice aliceChan)
  t2 <- async (tap bob bobChan)
  -- until cracking succeeds show the encrypted streams in real time
  t3 <- async $ do
    withAsync (forever $ Alice <$> readChan aliceSideChan >>= writeChan sideChan) $ \a1 -> do
    withAsync (forever $ Bob <$> readChan bobSideChan >>= writeChan sideChan) $ \a2 -> do
      forever $ do
        x <- readChan sideChan
        print x
  -- when secret is available, cancel the useless report and show the plaintext
  -- starting from beginning of conversation
  async $ do
    secret <- wait futureSecret
    let ss0 = initializeStreamCipher secret
    cancel t3
    async $ loopWith ss0 $ \loop ss -> do
      bytes <- readChan aliceChan
      let (text, ss') = crypt ss bytes
      BS.putStr ("Alice: " <> text <> "\n")
      loop ss'
    async $ loopWith ss0 $ \loop ss -> do
      bytes <- readChan bobChan
      let (text, ss') = crypt ss bytes
      BS.putStr ("Bob: " <> text <> "\n")
      loop ss'
  r <- waitCatch t1 :: IO (Either SomeException ())
  print r
  r <- waitCatch t2 :: IO (Either SomeException ())
  print r
  putStrLn "Eve: communications have ended both ways"

data AB a = Alice a | Bob a deriving (Show)

tap h ch = do
  bytes <- BS.hGetSome h 256
  BS.hPutStr h bytes
  writeChan ch bytes
  tap h ch

loopWith s0 f = f (\s -> loopWith s f) s0

hGetLineEve :: Handle -> Handle -> IO String
hGetLineEve h1 h2 = do
  x <- hGetLine h1
  hPutStrLn h2 x
  return x
