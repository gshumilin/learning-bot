{-# LANGUAGE DeriveGeneric #-}

module Types.Config where

import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics
import System.IO (FilePath)
import Types.Log (LogLvl (..))
import Types.Message

data Config = Config
  { frontEndType :: FrontEndType,
    logLvl :: LogLvl,
    logPath :: FilePath,
    tgToken :: Text,
    tgRequestHost :: Text,
    tgRequestPort :: Int,
    tgTimeout :: Int,
    defaultRepeatValue :: Int,
    helpText :: Text,
    repeatText :: Text,
    unknownText :: Text
  }
  deriving (Generic, Show)

instance FromJSON Config where
  parseJSON (Object o) = do
    someFrontEndType <- o .: "frontEndType" :: Parser Text
    let frontEndType = case someFrontEndType of
          "console" -> ConsoleFrontEnd
          "telegram" -> TelegramFrontEnd
    logLvl <- o .: "logLvl"
    logPath <- o .: "logPath"
    tgToken <- o .: "tgToken"
    tgRequestHost <- o .: "tgRequestHost"
    tgRequestPort <- o .: "tgRequestPort"
    tgTimeout <- o .: "tgTimeout"
    defaultRepeatValue <- o .: "defaultRepeatValue"
    helpText <- o .: "helpText"
    repeatText <- o .: "repeatText"
    unknownText <- o .: "unknownText"
    pure Config {..}

data FrontEndType = ConsoleFrontEnd | TelegramFrontEnd deriving (Show)

instance FromJSON FrontEndType where
  parseJSON (String s) = return ConsoleFrontEnd
