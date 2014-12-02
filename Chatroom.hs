{-# LANGUAGE RecordWildCards #-}

module Chatroom where

import           Control.Applicative
import           Control.Concurrent.STM
import qualified Data.Set as S

import Client
import Types

{-
    Chatroom
-}
data Chatroom = Chatroom
    { chatroomName          :: ChatroomName
    , chatroomRef           :: ChatroomRef
    , chatroomClients       :: TVar (S.Set ClientJoinID)
    , chatroomBroadcastChan :: TChan String
    }

newChatroom :: ChatroomName -> ChatroomRef -> STM Chatroom
newChatroom name ref = Chatroom name <$> return ref <*> newTVar S.empty <*> newBroadcastTChan

chatroomAddClient :: Chatroom -> ClientJoinID -> STM ()
chatroomAddClient room joinID = modifyTVar (chatroomClients room) . S.insert $ joinID

-- Send a Message to the channel.
chatroomMessage :: Chatroom -> String -> STM ()
chatroomMessage = writeTChan . chatroomBroadcastChan

-- Send a Broadcast to the channel, from a client.
chatroomBroadcast :: Chatroom -> ClientName -> String -> STM ()
chatroomBroadcast room@Chatroom{..} clientName msg = chatroomMessage room msg