module Types.Update where

import Control.Monad (mzero)
import Data.Text (Text)
import Data.Aeson
import Types.Message (Message)

data UpdatesRespond = UpdatesRespond
  { status :: Bool
  , updates :: [Update]
  } deriving (Show)

instance FromJSON UpdatesRespond where
  parseJSON (Object o) = do
    status <- o .: "ok"
    updates <- o .: "result"
    return $ UpdatesRespond{..}
  parseJSON _ = mzero

data Update = Update
  { updateId :: Int
  , updChatId :: Int
  , message :: Message
  } deriving (Show)

instance FromJSON Update where
  parseJSON (Object o) = do
    updateId <- o .: "update_id"
    msg <- o .: "message"
    chat <- msg .: "chat"
    updChatId <- chat .: "id"
    message <- o .: "message"
    return Update {..}
  parseJSON _ = mzero