{-#LANGUAGE RecordWildCards #-}

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
    , roomNameToRef   :: TVar (M.Map ChatroomName ChatroomRef)
    , serverChatrooms :: TVar (M.Map ChatroomRef Chatroom)
    , nameToJoinId    :: TVar (M.Map ClientName ClientJoinID)
    , serverClients   :: TVar (M.Map ClientJoinID Client)
    }

newChatServer :: String -> String -> IO ChatServer
newChatServer address port = atomically $ do
    ChatServer <$> return address <*> return port <*> newTVar 0 <*> newTVar 0 <*> newTVar M.empty
               <*> newTVar M.empty <*> newTVar M.empty <*> newTVar M.empty

addChatroom :: ChatServer -> ChatroomName -> ChatroomRef -> STM ()
addChatroom ChatServer{..} name roomRef = do
    room <- newChatroom name roomRef
    modifyTVar serverChatrooms . M.insert roomRef $ room
    modifyTVar roomNameToRef . M.insert name $ roomRef

lookupChatroomByName :: ChatServer -> ChatroomName -> STM (Maybe Chatroom)
lookupChatroomByName ChatServer{..} name = do
    roomRef <- M.lookup name <$> readTVar roomNameToRef
    case roomRef of
        Nothing  -> return Nothing
        Just ref -> M.lookup ref <$> readTVar serverChatrooms

lookupChatroomByRef :: ChatServer -> ChatroomRef -> STM (Maybe Chatroom)
lookupChatroomByRef ChatServer{..} roomRef = M.lookup roomRef <$> readTVar serverChatrooms

lookupOrCreateChatroom :: ChatServer -> ChatroomName -> STM Chatroom
lookupOrCreateChatroom server@ChatServer{..} name = lookupChatroomByName server name >>= \x ->
    case x of
      Nothing -> do
        roomRef <- readTVar roomRefCount
        room <- newChatroom name roomRef
        modifyTVar serverChatrooms . M.insert roomRef $ room
        modifyTVar roomNameToRef . M.insert name $ roomRef
        incrementRoomRefCount roomRefCount
        return room
      Just room -> return room

addClientToServer :: ChatServer -> ClientJoinID -> Client -> STM ()
addClientToServer server@ChatServer{..} joinID client@Client{..} =
    modifyTVar serverClients $ M.insert joinID client

removeClientFromServer :: ChatServer -> ClientJoinID -> STM ()
removeClientFromServer server@ChatServer{..} joinID =
    modifyTVar serverClients $ M.delete joinID

clientHandler :: Handle -> ChatServer -> IO ()
clientHandler handle server@ChatServer{..} =
    forever $ do
        msg <- hGetLine handle
        print $ msg ++ "!ENDLINE!"
        let cmd = words msg

        case (head cmd) of
            ("JOIN_CHATROOM:") -> joinCommand handle server msg
            ("CHAT:") -> messageCommand handle server msg
            ("LEAVE_CHATROOM:") -> leaveCommand handle server msg
            ("DISCONNECT:") -> terminateCommand handle server msg
            ("HELO") -> heloCommand handle server $ unwords $ tail cmd
            ("KILL_SERVICE") -> killCommand handle
            _ -> do hPutStrLn handle ("Unknown Command - " ++ msg ++ "\n\n")

joinCommand :: Handle -> ChatServer -> String -> IO ()
joinCommand handle server@ChatServer{..} command = do
    --putStrLn $ "Joincommand - " ++ command

    --let clines = splitOn "\\n" command
    --    chatroomName = last $ splitOn ":" $ clines !! 0
    --    clientName = last $ splitOn ":" $ clines !! 3
    {-putStrLn $ show chatroomName
    putStrLn $ show clientName
    putStrLn $ chatroomName ++ " " ++ clientName-}
    let chatroomName = last $ splitOn ":" $ command
    msg <- hGetLine handle --
    msg <- hGetLine handle -- 
    msg <- hGetLine handle

    let clientName = last $ splitOn ":" $ msg

    joinID <- atomically $ readTVar clientJoinCount

    c <- atomically $ newClient joinID handle
    atomically $ addClientToServer server joinID c
    atomically $ incrementClientJoinCount clientJoinCount

    room <- atomically $ lookupOrCreateChatroom server chatroomName
    atomically $ chatroomAddClient room joinID handle

    hPutStrLn handle $ 
              "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++
              "SERVER_IP:" ++ address ++ "\n" ++
              "PORT:" ++ port ++ "\n" ++
              "ROOM_REF:" ++ show (chatroomGetRef room) ++ "\n" ++
              "JOIN_ID:" ++ show joinID ++ "\n\n"

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

    room <- atomically $ lookupChatroomByRef server $ read chatroomRef

    case room of
        Nothing -> hPutStrLn handle ("The room you have messaged does not exist!")
        Just room -> do
            clients <- atomically $ readTVar $ chatroomClients room
            let handleList = map snd $ M.toList clients
            let msg = "CHAT:" ++ chatroomRef ++ "\n" ++ "CLIENT_NAME:" ++ clientName ++ "\n" ++ "MESSAGE:" ++ show message ++ "\n\n"
            mapM_ (\h -> hPutStrLn h msg >> hFlush h) handleList

leaveCommand :: Handle -> ChatServer -> String -> IO ()
leaveCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = last $ splitOn ": " $ clines !! 0
        joinID = last $ splitOn ": " $ clines !! 1
        clientName = last $ splitOn ": " $ clines !! 2
    
    room <- atomically $ lookupChatroomByRef server $ read chatroomRef

    case room of
        (Just r) -> do
            atomically $ chatroomRemoveClient r (read joinID)
                    
            hPutStrLn handle $ 
                "LEFT_CHATROOM:" ++ chatroomRef ++ "\n" ++
                "JOIN_ID:" ++ show joinID ++ "\n\n"
                    
            hFlush handle
        Nothing  -> do
            hPutStrLn handle ("Chatroom you have tried to leave does not exist.")
            hFlush handle
{-
Client Sends:
LEAVE_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
CLIENT_NAME: [string Handle to identifier client user]

Server Response:
LEFT_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
-}

terminateCommand :: Handle -> ChatServer -> String -> IO ()
terminateCommand handle server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        address = last $ splitOn ": " $ clines !! 0
        port = last $ splitOn ": " $ clines !! 1
        clientName = last $ splitOn ": " $ clines !! 2

    --atomically $ removeClientFromServer (read joinID)
    print $ "Client " ++ clientName ++ " removed!"
    hClose handle
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
                     "IP:" ++ "134.226.32.10" ++ "\n" ++
                     "Port:" ++ port ++ "\n" ++
                     "StudentID:11396966\n\n"

  hFlush handle

killCommand :: Handle -> IO ()
killCommand handle = do
    hPutStrLn handle "Service is now terminating!"
    exitSuccess

incrementClientJoinCount :: TVar ClientJoinID -> STM ()
incrementClientJoinCount tv = modifyTVar tv ((+) 1)

incrementRoomRefCount :: TVar ChatroomRef -> STM ()
incrementRoomRefCount tv = modifyTVar tv ((+) 1)