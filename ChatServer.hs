{-#LANGUAGE RecordWildCards #-}

import Network hiding (accept)
import Network.Socket hiding (Broadcast)
import System.Environment
import System.IO
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join)
import Data.List.Split
import Data.Word
import Data.Map hiding (split)
import Control.Concurrent.Async
import Prelude hiding (null, lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Maybe

maxThreadCount = 16

type ClientName = String
type ChatroomName = String
type RoomName = String

data ChatServer = ChatServer {
    address :: String,
    port :: String,
    chatrooms :: TVar (Map ChatroomName Chatroom),
    clients :: TVar (Map ClientName Client)
} deriving (Eq)

newChatServer :: String -> String -> IO ChatServer
newChatServer a p = do
    ChatServer <$> return a
               <*> return p
               <*> newTVarIO empty
               <*> newTVarIO empty

data Chatroom = Chatroom {
    roomName :: ChatroomName,
	roomClients :: TVar (Map ClientName Client)
} deriving (Eq)

newChatroom :: ChatroomName -> STM Chatroom
newChatroom name = do
    Chatroom <$> return name
             <*> newTVar empty

data Client = Client {
    clientName     :: ClientName,
    clientHandle   :: Handle,
    clientSendChan :: TChan String,
    clientKicked   :: TVar (Maybe String)
} deriving (Eq)

newClient :: ClientName -> Handle -> STM Client
newClient name handle = do
    Client <$> return name
           <*> return handle
           <*> newTChan
           <*> newTVar Nothing

sendMessage :: Client -> String -> STM ()
sendMessage Client{..} msg =
  writeTChan clientSendChan msg

broadcast :: ChatServer -> String -> STM ()
broadcast ChatServer{..} msg = do
  clientmap <- readTVar clients
  mapM_ (\client -> sendMessage client msg) (elems clientmap)

addClient :: ChatServer -> ClientName -> Handle -> IO (Maybe Client)
addClient server@ChatServer{..} name handle = atomically $ do
  clientmap <- readTVar clients
  if member name clientmap
    then return Nothing
    else do client <- newClient name handle
            writeTVar clients $ insert name client clientmap
            broadcast server  $ (name ++ " has connected")
            return (Just client)

addClientToRoom :: ChatServer -> ClientName -> Handle -> RoomName -> IO (Maybe Client)
addClientToRoom server@ChatServer{..} name handle roomName = atomically $ do
  roomsMap <- readTVar chatrooms
  let room = lookup roomName roomsMap

  -- if room does not exist, create it and add it to the chatroom map, otherwise do nothing
  if (room == Nothing)
    then do newRoom <- newChatroom roomName
            writeTVar chatrooms $ insert roomName newRoom roomsMap
    else return ()

  roomsMap' <- readTVar chatrooms
  let room' = lookup roomName roomsMap
  roomClientsMap <- readTVar $ roomClients $ fromJust room'

  let client = lookup name roomClientsMap

  -- if client does not exist, add it, otherwise return Nothing to signify it already exists
  if (client == Nothing)
    then do newClient <- newClient name handle
            writeTVar (roomClients $ fromJust room') $ insert name newClient roomClientsMap
            return (Just newClient)
    else return (Nothing)

removeClient :: ChatServer -> ClientName -> IO ()
removeClient server@ChatServer{..} name = atomically $ do
  modifyTVar' clients $ delete name
  broadcast server $ (name ++ " has disconnected")

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
    _ -> mainHandler sock chan

acceptHandler :: Socket -> Chan String -> TVar Int -> ChatServer -> IO ()
acceptHandler sock chan threadCount server = forever $ do
  (s, addr) <- accept sock
  handle <- socketToHandle s ReadWriteMode
  hSetNewlineMode handle universalNewlineMode
  --hSetBuffering handle NoBuffering

  count <- atomically $ readTVar threadCount
  putStrLn $ "threadCount = " ++ show count

  if (count < maxThreadCount) then do
    myForkFinally (clientHandler handle chan server threadCount) (\_ -> atomically $ decrementTVar threadCount)
    atomically $ incrementTVar threadCount
    else do
      hPutStrLn handle "Service reached maximum capacity, please try again later!"
      hClose handle

clientHandler :: Handle -> Chan String -> ChatServer -> TVar Int -> IO ()
clientHandler handle chan server threadCount = forever $ do
  line <- hGetLine handle
  let cmd = words line

  case (head cmd) of
    ("JOIN_CHATROOM:") -> joinCommand handle chan server line
    ("HELO") -> heloCommand handle chan server $ unwords $ tail cmd
    ("KILL_SERVICE") -> killCommand handle chan
    _ -> do hPutStrLn handle ("Unknown Command - " ++ line)

joinCommand :: Handle -> Chan String -> ChatServer -> String -> IO ()
joinCommand handle chan server@ChatServer{..} command = do
    putStrLn $ "Joincommand - " ++ command

    let clines = splitOn "\\n" command
        chatroomName = last $ splitOn ": " $ clines !! 0
        clientName = last $ splitOn ": " $ clines !! 3
    putStrLn $ show chatroomName
    putStrLn $ show clientName
    putStrLn $ chatroomName ++ " " ++ clientName

    --addClientToRoom server clientName handle chatroomName

    --putStrLn $ test

    hPutStrLn handle $ "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
              "SERVER_IP:" ++ address ++ "\n" ++
              "PORT:" ++ port ++ "\n" ++
              "ROOM_REF:" ++ show 1 ++ "\n" ++
              "JOIN_ID:" ++ show 1 ++ "\n"
{-
Error Response:
ERROR_CODE: [integer]
ERROR_DESCRIPTION: [string describing error]
-}

leaveCommand :: undefined
leaveCommand = undefined
{-
Client Sends:
LEAVE_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
CLIENT_NAME: [string Handle to identifier client user]

Server Response:
LEFT_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
-}

terminateCommand :: undefined
terminateCommand = undefined
{-
Client Sends:
DISCONNECT: [IP address of client if UDP | 0 if TCP]
PORT: [port number of client it UDP | 0 id TCP]
CLIENT_NAME: [string handle to identify client user]

Server closes connection
-}

messageCommand :: undefined
messageCommand = undefined
{-
Client Sends:
CHAT: [ROOM_REF]
JOIN_ID: [integer identifying client to server]
CLIENT_NAME: [string identifying client user]
MESSAGE: [string terminated with '\n\n']

Server Responds to all clients:
CHAT: [ROOM_REF]
CLIENT_NAME: [string identifying client user]
MESSAGE: [string terminated with '\n\n']
-}

heloCommand :: Handle -> Chan String -> ChatServer -> String -> IO ()
heloCommand handle chan ChatServer{..} msg = do
  writeChan chan "HELO command processed!"

  hPutStrLn handle $ "HELO " ++ msg ++ "\n" ++
                     "IP:" ++ address ++ "\n" ++
                     "Port:" ++ port ++ "\n" ++
                     "StudentID:11396966\n"

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