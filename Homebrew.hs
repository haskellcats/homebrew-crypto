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

main = do
  args <- getArgs
  -- either wait for a connection or initial a connection
  client <- case args of
    ["-g", size] -> do
      dh <- generateGroup DHStart (read size)
      saveDH "diffie-hellman-params" dh
      exitSuccess
    ["-p", port] -> do
      server <- listenOn (PortNumber (fromIntegral (read port)))
      (client,_,_) <- accept server
      sClose server
      return client
    [host, port] -> do
      connectTo host (PortNumber (fromIntegral (read port)))
    _ -> do
      putStrLn "usage: <command> -p port, <command> hostname port, <command> -g bits"
      exitSuccess
  dh <- loadDH <$> readFile "diffie-hellman-params"
  -- begin DH handshake
  (alice, k_a) <- dhDialog dh
  hPutStrLn client (show alice)
  bob <- read <$> hGetLine client :: IO Integer
  let sharedSecret = k_a bob -- shared secret established
  -- pair of stream ciphers initialized with shared secret
  let ssIn0 = initializeStreamCipher sharedSecret
  let ssOut0 = initializeStreamCipher sharedSecret
  -- two loops, one to send and one to recieve
  forkIO $ loopWith ssIn0 $ \loop ss -> do
    ctext <- BS.hGetSome client 1024
    let (text, ss') = crypt ss ctext
    BS.putStr text
    loop ss'
  loopWith ssOut0 $ \loop ss -> do
    text <- encodeUtf8 . T.pack . (++ "\n") <$> getLine
    let (ctext, ss') = crypt ss text
    BS.hPutStr client ctext
    loop ss'
      
loopWith s0 f = f (\s -> loopWith s f) s0
