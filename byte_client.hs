module Main where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, sendAll)
import System.IO

import Control.Concurrent

main :: IO ()                                    
main = do                                        
  sock <- socket AF_INET Stream 0 -- create a new sock
  connect sock (SockAddrInet 4000 0)
  --get init messages from server
  --then receive all messages from server
  --while ocassionally sending to server
  -- chan <- newChan
  forkIO(recvLoop sock)
  sendLoop sock
  close sock

-- sendLoop :: Socket -> Chan Int -> IO ()                                    
sendLoop :: Socket -> IO ()                                    
sendLoop sock = do
  msg_string <- getLine
  sendAll sock $ C.pack msg_string
  case msg_string of
    "!quit" -> putStrLn "exiting..."
    _ -> do 
      sendLoop sock

-- recvLoop :: Socket -> Chan Int -> IO ()
recvLoop :: Socket -> IO ()
recvLoop sock = do
  msg <- recv sock 1024
  print("Received >>> " ++ C.unpack msg)
  -- case msg_string of
  --   "goodbye" -> putStrLn "exiting..."
  --   _ -> do 
  --     recvLoop sock
  recvLoop sock

-- clientLoop :: Socket -> IO ()
-- clientLoop sock = do
--   clientLoop sock