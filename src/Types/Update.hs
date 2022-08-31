module Types.Update where

import Control.Monad (mzero)
import Data.Aeson
import Data.Foldable (asum)
import Data.Text (Text)
import Types.Message (Message)

data UpdatesRespond = UpdatesRespond
  { status :: Bool,
    updates :: [Update]
  }
  deriving (Show)

instance FromJSON UpdatesRespond where
  parseJSON (Object o) = do
    status <- o .: "ok"
    updates <- o .: "result"
    return $ UpdatesRespond {..}
  parseJSON _ = mzero

data Update = Update
  { updateId :: Int,
    updChatId :: Int,
    message :: Message
  }
  deriving (Show)

instance FromJSON Update where
  parseJSON (Object o) =
    asum
      [ do
          updateId <- o .: "update_id"
          messageObj <- o .: "message"
          chat <- messageObj .: "chat"
          updChatId <- chat .: "id"
          message <- o .: "message"
          return Update {..},
        do
          updateId <- o .: "update_id"
          message <- o .: "callback_query"
          messageObj <- o .: "callback_query"
          from <- messageObj .: "from"
          updChatId <- from .: "id"
          return Update {..}
      ]
  parseJSON _ = mzero
