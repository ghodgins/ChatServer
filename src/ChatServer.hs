{-#LANGUAGE RecordWildCards #-}

module ChatServer where

import Network hiding (accept, sClose)
import Network.Socket hiding (send, recv, sendTo, recvFrom, Broadcast)
import Network.Socket.ByteString
import Data.ByteString.Char8 (pack, unpack)
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

clientHandler :: Socket -> ChatServer -> IO ()
clientHandler sock server@ChatServer{..} =
    forever $ do
        message <- recv sock 1024
	let msg = unpack message
        print $ msg ++ "!ENDLINE!"
        let cmd = head $ words $ head $ splitOn ":" msg
        print cmd

        case cmd of
            ("JOIN_CHATROOM") -> joinCommand sock server msg
            ("CHAT") -> messageCommand sock server msg
            ("LEAVE_CHATROOM") -> leaveCommand sock server msg
            ("DISCONNECT") -> terminateCommand sock server msg
            ("HELO") -> heloCommand sock server $ (words msg) !! 1
            ("KILL_SERVICE") -> killCommand sock
            _ -> do send sock (pack ("Unknown Command - " ++ msg ++ "\n\n")) ; return ()

joinCommand :: Socket -> ChatServer -> String -> IO ()
joinCommand sock server@ChatServer{..} command = do
    --putStrLn $ "Joincommand - " ++ command

    let clines = splitOn "\\n" command
        chatroomName = (splitOn ":" $ clines !! 0) !! 1
        clientName = (splitOn ":" $ clines !! 3) !! 1
    {-putStrLn $ show chatroomName
    putStrLn $ show clientName
    putStrLn $ chatroomName ++ " " ++ clientName-}

    joinID <- atomically $ readTVar clientJoinCount

    c <- atomically $ newClient joinID sock
    atomically $ addClientToServer server joinID c
    atomically $ incrementClientJoinCount clientJoinCount

    room <- atomically $ lookupOrCreateChatroom server chatroomName
    atomically $ chatroomAddClient room joinID sock

    sendAll sock $ pack $
         "JOINED_CHATROOM:" ++ chatroomName ++ "\n" ++ 
         "SERVER_IP:" ++ address ++ "\n" ++
         "PORT:" ++ port ++ "\n" ++
         "ROOM_REF:" ++ show (chatroomGetRef room) ++ "\n" ++
         "JOIN_ID:" ++ show joinID ++ "\n\n"

    return ()
    --hFlush sock
{-
Error Response:
ERROR_CODE: [integer]
ERROR_DESCRIPTION: [string describing error]
-}

messageCommand :: Socket -> ChatServer -> String -> IO ()
messageCommand sock server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = (splitOn ":" $ clines !! 0) !! 1
        joinID = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1
        message = (splitOn ":" $ clines !! 3) !! 1

    {-putStrLn $ show chatroomRef
    putStrLn $ show joinID
    putStrLn $ show clientName
    putStrLn $ show message-}

    room <- atomically $ lookupChatroomByRef server $ read chatroomRef

    case room of
        Nothing -> send sock (pack "The room you have messaged does not exist!") >> return ()
        Just room -> do
            clients <- atomically $ readTVar $ chatroomClients room
            let sockList = map snd $ M.toList clients
            let msg = "CHAT:" ++ chatroomRef ++ "\n" ++ "CLIENT_NAME:" ++ clientName ++ "\n" ++ "MESSAGE:" ++ show message ++ "\n\n"
            --mapM_ (\h -> hPutStrLn h msg >> hFlush h) handleList
            mapM_ (\s -> sendAll s (pack msg)) sockList
            return ()

leaveCommand :: Socket -> ChatServer -> String -> IO ()
leaveCommand sock server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        chatroomRef = (splitOn ":" $ clines !! 0) !! 1
        joinID = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1
    
    room <- atomically $ lookupChatroomByRef server $ read chatroomRef

    case room of
        (Just r) -> do
            atomically $ chatroomRemoveClient r (read joinID)
                    
            send sock $ pack $
                "LEFT_CHATROOM:" ++ chatroomRef ++ "\n" ++
                "JOIN_ID:" ++ show joinID ++ "\n\n"

            return ()
                    
            --hFlush sock
        Nothing  -> do
            send sock (pack "Chatroom you have tried to leave does not exist.")
            return ()
            --hFlush sock
{-
Client Sends:
LEAVE_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
CLIENT_NAME: [string sock to identifier client user]

Server Response:
LEFT_CHATROOM: [ROOM_REF]
JOIN_ID: [integer previously provided by server on join]
-}

terminateCommand :: Socket -> ChatServer -> String -> IO ()
terminateCommand sock server@ChatServer{..} command = do
    let clines = splitOn "\\n" command
        address = (splitOn ":" $ clines !! 0) !! 1
        port = (splitOn ":" $ clines !! 1) !! 1
        clientName = (splitOn ":" $ clines !! 2) !! 1

    --atomically $ removeClientFromServer (read joinID)
    print $ "Client " ++ clientName ++ " removed!"
    sClose sock
{-
Client Sends:
DISCONNECT: [IP address of client if UDP | 0 if TCP]
PORT: [port number of client it UDP | 0 id TCP]
CLIENT_NAME: [string sock to identify client user]

Server closes connection
-}

heloCommand :: Socket -> ChatServer -> String -> IO ()
heloCommand sock ChatServer{..} msg = do
  send sock $ pack $ "HELO " ++ msg ++ "\n" ++
                     "IP:" ++ "134.226.32.10" ++ "\n" ++
                     "Port:" ++ port ++ "\n" ++
                     "StudentID:11396966\n\n"

  return ()
  --hFlush sock

killCommand :: Socket -> IO ()
killCommand sock = do
    send sock $ pack $ "Service is now terminating!"
    exitSuccess

incrementClientJoinCount :: TVar ClientJoinID -> STM ()
incrementClientJoinCount tv = modifyTVar tv ((+) 1)

incrementRoomRefCount :: TVar ChatroomRef -> STM ()
incrementRoomRefCount tv = modifyTVar tv ((+) 1)
