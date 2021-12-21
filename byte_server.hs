module Main where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding     (recv)
import Network.Socket.ByteString (recv, send, sendAll)

import Control.Monad (when)
import Control.Concurrent
import Control.Concurrent (killThread)

--create a sock, bind it, and listen for connections
main :: IO ()                                    
main = do                                        
  sock <- socket AF_INET Stream 0 -- create a new sock
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4000 0)                
  chan <- newChan -- create a channel
  listenLoop sock chan 0

type Message = (Int, String)

--listen that listens to clients
listenLoop :: Socket -> Chan Message -> Int -> IO ()
listenLoop sock chan ident = do
  listen sock 2                                  
  putStrLn "Listening on port 4000..."           
  (conn, _) <- accept sock                       
  putStrLn "New connection found."             
  sendAll conn $ C.pack "Welcome to server -- type '!quit' to quit" 
  forkIO(dealWithConnection conn chan ident) -- pass it to all threads
  listenLoop sock chan $! ident + 1

dealWithConnection :: Socket -> Chan Message -> Int -> IO ()
dealWithConnection conn chan ident = do

  reading_thread <- forkIO(readChannel conn chan ident)

  msg_bytes <- recv conn 1024
  let msg_string = C.unpack msg_bytes
  case msg_string of
    "!quit" -> do
      putStrLn "server recv quit, close connection"
      sendAll conn $ C.pack "goodbye"
      close conn
    -- "!help" -> do
    --   provide_help
    _ -> do
      -- putStrLn("1")
      writeChan chan (ident, msg_string)
      dealWithConnection conn chan ident
  
  killThread reading_thread

readChannel :: Socket -> Chan Message -> Int -> IO ()
readChannel conn chan ident = do
  (next_msg_ident, msg_from_chan) <- readChan chan 
  when (ident /= next_msg_ident) $ sendAll conn $ C.pack msg_from_chan
  readChannel conn chan ident
