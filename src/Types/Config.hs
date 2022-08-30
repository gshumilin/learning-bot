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
  , helpText :: Text
  , repeatText :: Text
  , unknowText :: Text
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON (Object o) = do  
    someFrontEndType <- o .: "frontEndType" :: Parser Text
    let frontEndType = case someFrontEndType of
          "console" -> ConsoleFrontEnd
          "telegram" -> TelegramFrontEnd
    tgToken <- o .: "tgToken"
    tgRequestHost <- o .: "tgRequestHost"
    tgRequestPort <- o .: "tgRequestPort"
    tgTimeout <- o .: "tgTimeout"
    logPath <- o .: "logPath"
    defaultRepeatValue <- o .: "defaultRepeatValue"
    helpText <- o .: "helpText"
    repeatText <- o .: "repeatText"
    unknowText <- o .: "unknowText"
    pure Config {..}

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd deriving Show

instance FromJSON FrontEndType where
  parseJSON (String s) = return ConsoleFrontEnd