{-# LANGUAGE RecordWildCards #-}

module Chatroom where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Data.Map as M
import System.IO

import Client
import Types

{-
    Chatroom
-}
data Chatroom = Chatroom
    { chatroomName          :: ChatroomName
    , chatroomRef           :: ChatroomRef
    , chatroomClients       :: TVar (M.Map ClientJoinID Handle)
    }

newChatroom :: ChatroomName -> ChatroomRef -> STM Chatroom
newChatroom name ref = Chatroom name <$> return ref <*> newTVar M.empty

chatroomAddClient :: Chatroom -> ClientJoinID -> Handle -> STM ()
chatroomAddClient room joinID handle = modifyTVar (chatroomClients room) . M.insert joinID $ handle