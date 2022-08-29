{-# LANGUAGE DeriveGeneric #-}

module Types.Config where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics
import Types.Message 

data Config = Config
  { frontEndType :: FrontEndType
  , tgToken :: Text
  , tgRequestHost :: Text
  , tgRequestPort :: Int
  , tgTimeout :: Int
  , logPath :: Text
  , defaultRepeatValue :: Int
  , commandMessages :: ConfCommandMessages 
  } deriving (Generic, Show)
instance FromJSON Config

data ConfCommandMessages = ConfCommandMessages
  { help      :: ServiceMessage
  , repeat    :: ServiceMessage
  , askRepeat :: ServiceMessage
  , unknown   :: ServiceMessage
  } deriving (Generic, Show)
instance FromJSON ConfCommandMessages

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd deriving Show

instance FromJSON FrontEndType where
  parseJSON (Object o) = do
    frontEnd <- o .: "frontEndType" :: Parser Text
    case frontEnd of
      "console" -> return ConsoleFrontEnd
      "telegram" -> return TelegramFrontEnd