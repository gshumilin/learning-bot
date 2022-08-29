module Types.Update where

import Control.Monad (mzero)
import Data.Text (Text)
import Data.Aeson
import Types.Message (Message)

data Updates = Updates
  { updatesStatus :: Bool
  , updates :: [Update]
  } deriving (Show)

instance FromJSON Updates where
  parseJSON (Object o) = do
    updatesStatus <- o .: "ok"
    updates <- o .: "result"
    return $ Updates{..}
  parseJSON _ = mzero

data Update = Update
  { updateId :: Int
  , chatId :: Int
  , updateContent :: Message
  } deriving (Show)

instance FromJSON Update where
  parseJSON (Object o) = do
    updateId <- o .: "update_id"
    msg <- o .: "message"
    chatId <- msg .: "chat_id"
    updateContent <- o .: "message"
    return Update {..}
  parseJSON _ = mzero