module Main where
	
import Network hiding (accept)
import Network.Socket
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Data.List.Split
import Data.Word

data Server = Server { address :: String, port :: String }

newServer :: String -> String -> Server
newServer a p = Server { address = a, port = p }

maxThreadCount = 16

main:: IO ()
main = withSocketsDo $ do
  args <- getArgs
  let address = args !! 0
  let port = args !! 1

  sock <- listenOn $ PortNumber $ fromIntegral $ read port

  putStrLn $ "Listening on " ++ port

  chan <- newChan
  threadCount <- atomically $ newTVar 0

  let server = newServer address port

  forkIO $ acceptHandler sock chan threadCount server

  mainHandler sock chan

mainHandler :: Socket -> Chan String -> IO ()
mainHandler sock chan = do
  chanMsg <- readChan chan

  case (chanMsg) of
    ("KILL_SERVICE") -> putStrLn "Service is now terminating!"
    _ -> mainHandler sock chan

acceptHandler :: Socket -> Chan String -> TVar Int -> Server -> IO ()
acceptHandler sock chan threadCount server = do
  (s, addr) <- accept sock
  handle <- socketToHandle s ReadWriteMode
  --hSetBuffering handle NoBuffering

  count <- atomically $ readTVar threadCount
  putStrLn $ "threadCount = " ++ show count

  if (count < maxThreadCount) then do
    myForkFinally (clientHandler handle chan server threadCount) (\_ -> atomically $ decrementTVar threadCount)
    atomically $ incrementTVar threadCount
    else do
      hPutStrLn handle "Service reached maximum capacity, please try again later!"
      hClose handle

  acceptHandler sock chan threadCount server

clientHandler :: Handle -> Chan String -> Server -> TVar Int -> IO ()
clientHandler handle chan server threadCount = do
  line <- hGetLine handle
  let cmd = words line

  case (head cmd) of
    ("HELO") -> heloCommand handle chan server threadCount (unwords (tail cmd))
    ("KILL_SERVICE") -> killCommand handle chan
    _ -> do hPutStrLn handle ("Unknown Command - " ++ line)
  
  clientHandler handle chan server threadCount

heloCommand :: Handle -> Chan String -> Server -> TVar Int -> String -> IO ()
heloCommand handle chan server@Server{..} threadCount msg = do
  writeChan chan "HELO command processed!"

  hPutStrLn handle $ "HELO " ++ msg ++ "\n\
                     \IP:" ++ address ++ "\n\
                     \Port:" ++ port ++ "\n\
                     \StudentID:11396966"

  hFlush handle

killCommand :: Handle -> Chan String -> IO ()
killCommand handle chan = do
  hPutStrLn handle "Service is now terminating!"
  writeChan chan "KILL_SERVICE"

incrementTVar :: TVar Int -> STM ()
incrementTVar tv = modifyTVar tv ((+) 1)

decrementTVar :: TVar Int -> STM ()
decrementTVar tv = modifyTVar tv (subtract 1)

myForkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
myForkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then