module Main where

import Network hiding (accept)
import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join)
import Data.List.Split
import qualified Data.Map as M hiding (split)
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
    server <- newChatServer "localhost" serverport
    --sock <- listenOn (PortNumber (fromIntegral serverport))

    addrinfos <- getAddrInfo
                 (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                 Nothing (Just serverport)

    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    listen sock 5

    _ <- printf "Listening on port %s\n" serverport

    threadCount <- atomically $ newTVar 0

    forever $ do
        count <- atomically $ readTVar threadCount
        putStrLn $ "threadCount = " ++ show count

        (clientSock, connAddr) <- accept sock
        _ <- printf "Accepted connection from %s\n" (show connAddr)

        if (count < maxThreadCount) then do
            myForkFinally (clientHandler clientSock server) (\_ -> atomically $ decrementTVar threadCount)
            atomically $ incrementTVar threadCount
            else do
                send clientSock "Service reached maximum capacity, please try again later!"
                close clientSock

serverport :: String
serverport = "44444"

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then