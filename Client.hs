{-# LANGUAGE RecordWildCards, TupleSections #-} 

module Client where

import Control.Concurrent.STM
import System.IO (Handle, hPutStrLn)
import qualified Data.Set as S

import Types

data Client = Client
    { clientName          :: TVar ClientName
    , clientJoinID        :: ClientJoinID
    , clientHandle        :: Handle
    , clientRoomRefs      :: TVar (S.Set ChatroomRef)
    }

newClient :: ClientJoinID -> Handle -> STM Client
newClient joinID handle = do
    clientName <- newTVar "default"
    roomRefs <- newTVar S.empty
    return Client
        { clientName          = clientName
        , clientJoinID        = joinID
        , clientHandle        = handle
        , clientRoomRefs      = roomRefs
        }

clientChangeName :: Client -> ClientName -> STM ()
clientChangeName client@Client{..} name = writeTVar clientName name 