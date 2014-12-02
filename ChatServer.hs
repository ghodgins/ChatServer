{-#LANGUAGE LambdaCase, RecordWildCards #-}

module ChatServer where

import Network hiding (accept)
import Network.Socket hiding (Broadcast)
import System.Environment
import System.IO
import System.Exit
import Control.Concurrent {- hiding (forkFinally) instead using myFOrkFinally to avoid GHC version issues-}
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, join, mapM, mapM_, forM, forM_, sequence, sequence_)
import Data.List.Split
import qualified Data.Map as M hiding (split)
import Control.Concurrent.Async
import Prelude hiding (null, lookup)
import Control.Applicative ((<$>), (<*>))

import Chatroom
import Client
import Types

{-
    ChatServer
-}
data ChatServer = ChatServer
    { address         :: String
    , port            :: String
    , clientJoinCount :: TVar ClientJoinID
    , roomRefCount    :: TVar ChatroomRef
    , serverChatrooms :: TVar (M.Map ChatroomRef Chatroom)
    , serverClients   :: TVar (M.Map ClientJoinID Client)
    }

newChatServer :: String -> String -> IO ChatServer
newChatServer address port = atomically $ do
    server <- ChatServer <$> return address <*> return port <*> newTVar 0 <*> newTVar 0 <*> newTVar M.empty <*> newTVar M.empty
    return server


addChatroom :: ChatServer -> ChatroomName -> ChatroomRef -> STM ()
addChatroom ChatServer{..} name roomRef = newChatroom name roomRef >>= modifyTVar serverChatrooms . M.insert roomRef

lookupChatroom :: ChatServer -> ChatroomRef -> STM (Maybe Chatroom)
lookupChatroom ChatServer{..} roomRef = M.lookup roomRef <$> readTVar serverChatrooms

lookupOrCreateChatroom :: ChatServer -> ChatroomName -> ChatroomRef -> STM Chatroom
lookupOrCreateChatroom server@ChatServer{..} name roomRef = lookupChatroom server roomRef >>= \case
    Nothing -> do
        room <- newChatroom name roomRef
        modifyTVar serverChatrooms . M.insert roomRef $ room
        --incrementRoomRefCount roomRefCount
        return room
    Just room -> return room

addClientToServer :: ChatServer -> ClientJoinID -> Client -> STM ()
addClientToServer server@ChatServer{..} joinID client@Client{..} =
    modifyTVar serverClients $ M.insert joinID client

clientHandler :: Handle -> ChatServer -> IO ()
clientHandler handle server@ChatServer{..} =
    forever $ do
        msg <- hGetLine handle
        let cmd = words msg

        case (head cmd) of
            ("JOIN_CHATROOM:") -> joinCommand handle server msg
            ("CHAT:") -> messageCommand handle server msg
            ("HELO") -> heloCommand handle server $ unwords $ tail cmd
            ("KILL_SERVICE") -> killCommand handle
            _ -> do hPutStrLn handle ("Unknown Command - " ++ msg)

joinCommand :: Handle -> ChatServer -> String -> IO ()
joinCommand handle server@ChatServer{..} command = do
    --putStrLn $ "Joincommand - " ++ command

    let clines = splitOn "\\n" command
        chatroomName = last $ splitOn ": " $ clines !! 0
        clientName = last $ splitOn ": " $ clines !! 3
    {-putStrLn $ show chatroomName
    putStrLn $ show clientName
    putStrLn $ chatroomName ++ " " ++ clientName-}

    joinID <- atomically $ readTVar clientJoinCount
    roomRef <- atomically $ readTVar roomRefCount 

    c <- atomically $ newClient joinID handle
    atomically $ addClientToServer server joinID c
    atomically $ incrementClientJoinCount clientJoinCount

    --atomically $ addChatroom server chatroomName roomRef
    room <- atomically $ lookupOrCreateChatroom server chatroomName roomRef
    atomically $ chatroomAddClient room joinID handle

    hPutStrLn handle $ 
              "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
              "SERVER_IP:" ++ address ++ "\n" ++
              "PORT:" ++ port ++ "\n" ++
              "ROOM_REF:" ++ show roomRef ++ "\n" ++
              "JOIN_ID:" ++ show joinID ++ "\n"

    hFlush handle
{-
Error Response:
ERROR_CODE: [integer]
ERROR_DESCRIPTION: [string describing error]
-}

messageCommand :: Handle -> ChatServer -> String -> IO ()
messageCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = last $ splitOn ": " $ clines !! 0
        joinID = last $ splitOn ": " $ clines !! 1
        clientName = last $ splitOn ": " $ clines !! 2
        message = last $ splitOn ": " $ clines !! 3

    {-putStrLn $ show chatroomRef
    putStrLn $ show joinID
    putStrLn $ show clientName
    putStrLn $ show message-}

    room <- atomically $ lookupChatroom server $ read chatroomRef

    case room of
        Nothing -> hPutStrLn handle $ "The room you have messaged does not exist!"
        Just room -> do
            clients <- atomically $ readTVar $ chatroomClients room
            let handleList = map snd $ M.toList clients
            let message = "CHAT:" ++ chatroomRef ++ "\n" ++ "CLIENT_NAME:" ++ clientName ++ "\n" ++ "MESSAGE:" ++ show message ++ "\n\n"
            return $ mapM_ hPutStrLn handleList message

    hFlush handle

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

heloCommand :: Handle -> ChatServer -> String -> IO ()
heloCommand handle ChatServer{..} msg = do
  hPutStrLn handle $ "HELO " ++ msg ++ "\n" ++
                     "IP:" ++ address ++ "\n" ++
                     "Port:" ++ port ++ "\n" ++
                     "StudentID:11396966\n"

  hFlush handle

killCommand :: Handle -> IO ()
killCommand handle = do
    hPutStrLn handle "Service is now terminating!"
    exitSuccess

incrementClientJoinCount :: TVar ClientJoinID -> STM ()
incrementClientJoinCount tv = modifyTVar tv ((+) 1)

incrementRoomRefCount :: TVar ChatroomRef -> STM ()
incrementRoomRefCount tv = modifyTVar tv ((+) 1)