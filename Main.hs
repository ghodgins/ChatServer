module Main where

import Network
import Network.Socket hiding (accept)
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join)
import Data.List.Split
import qualified Data.Map as M hiding (split)
import Control.Concurrent.Async
import Prelude hiding (null, lookup)
import Text.Printf (printf)


import ChatServer

{-
    Constants
-}
maxThreadCount :: Int
maxThreadCount = 16

main :: IO ()
main = withSocketsDo $ do
    server <- newChatServer "localhost" "44444"
    sock <- listenOn (PortNumber (fromIntegral serverport))
    _ <- printf "Listening on port %d\n" serverport

    threadCount <- atomically $ newTVar 0

    forever $ do
        count <- atomically $ readTVar threadCount
        putStrLn $ "threadCount = " ++ show count

        (handle, host, port') <- accept sock
        _ <- printf "Accepted connection from %s: %s\n" host (show port')

        if (count < maxThreadCount) then do
            myForkFinally (clientHandler handle server) (\_ -> atomically $ decrementTVar threadCount)
            atomically $ incrementTVar threadCount
            else do
                hPutStrLn handle "Service reached maximum capacity, please try again later!"
                hClose handle

serverport :: Int
serverport = 44444

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

{-import Network hiding (accept)
import Network.Socket hiding (Broadcast)
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Data.Map hiding (split)
import Control.Concurrent.Async
import Prelude hiding (null, lookup)


import ChatServer

{-
    Main and protocol functions
-}
main:: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let address = args !! 0
  let port = args !! 1

  sock <- listenOn $ PortNumber $ fromIntegral $ read port

  putStrLn $ "Listening on " ++ port

  chan <- newChan
  threadCount <- atomically $ newTVar 0

  server <- newChatServer address port

  forkIO $ acceptHandler sock chan threadCount server

  mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do
  chanMsg <- readChan chan

  case (chanMsg) of
    ("KILL_SERVICE") -> putStrLn "Service is now terminating!"
    _ -> mainHandler sock chan-}