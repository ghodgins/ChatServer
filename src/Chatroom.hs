{-# LANGUAGE RecordWildCards #-}

module Chatroom where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Data.Map as M
import System.IO
import Network.Socket

import Client
import Types

{-
    Chatroom
-}
data Chatroom = Chatroom
    { chatroomName          :: ChatroomName
    , chatroomRef           :: ChatroomRef
    , chatroomClients       :: TVar (M.Map ClientJoinID Socket)
    }

newChatroom :: ChatroomName -> ChatroomRef -> STM Chatroom
newChatroom name ref = Chatroom name <$> return ref <*> newTVar M.empty

chatroomAddClient :: Chatroom -> ClientJoinID -> Socket -> STM ()
chatroomAddClient room joinID sock = modifyTVar (chatroomClients room) . M.insert joinID $ sock

chatroomRemoveClient :: Chatroom -> ClientJoinID -> STM ()
chatroomRemoveClient room joinID = modifyTVar (chatroomClients room) $ M.delete joinID

chatroomGetRef :: Chatroom -> ChatroomRef
chatroomGetRef Chatroom{..} = chatroomRef