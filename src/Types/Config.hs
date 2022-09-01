module Types.Config where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Types.Log (LogLvl (..))

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
  deriving (Show)

instance FromJSON Config where
  parseJSON (Object o) = do
    someFrontEndType <- o .: "frontEndType" :: Parser Text
    let frontEndType = case someFrontEndType of
          "console" -> ConsoleFrontEnd
          "telegram" -> TelegramFrontEnd
          _ -> UnknownFrontend
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
  parseJSON _ = mzero

data FrontEndType = UnknownFrontend | ConsoleFrontEnd | TelegramFrontEnd deriving (Show)
