{-# LANGUAGE RecordWildCards, TupleSections #-} 

module Client where

import Control.Concurrent.STM
import System.IO (Handle, hPutStrLn)
import Network.Socket
import qualified Data.Set as S

import Types

data Client = Client
    { clientName          :: TVar ClientName
    , clientJoinID        :: ClientJoinID
    , clientSocket        :: Socket
    , clientRoomRefs      :: TVar (S.Set ChatroomRef)
    }

newClient :: ClientJoinID -> Socket -> STM Client
newClient joinID socket = do
    clientName <- newTVar "default"
    roomRefs <- newTVar S.empty
    return Client
        { clientName          = clientName
        , clientJoinID        = joinID
        , clientSocket        = socket
        , clientRoomRefs      = roomRefs
        }

clientChangeName :: Client -> ClientName -> STM ()
clientChangeName client@Client{..} name = writeTVar clientName name 