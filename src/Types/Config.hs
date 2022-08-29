{-# LANGUAGE DeriveGeneric #-}

module Types.Config where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics
import Types.Message (ServiceMessage()) 

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

data ConfCommandMessages = ConfCommandMessages
  { help    :: ServiceMessage
  , repeat  :: ServiceMessage
  , unknown :: ServiceMessage
  } deriving (Generic, Show)

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd deriving Show

instance FromJSON FrontEndType where
  parseJSON (Object o) = do
    frontEnd <- o .: "frontEndType" :: Parser Text
    case frontEnd of
      "console" -> return ConsoleFrontEnd
      "telegram" -> return TelegramFrontEnd