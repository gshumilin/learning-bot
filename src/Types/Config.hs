{-# LANGUAGE DeriveGeneric #-}

module Types.Config where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Log (LogDescType (..), LogLvl (..))

data Config = Config
  { frontEndType :: Text,
    logLvl :: LogLvl,
    logDescType :: LogDescType,
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

instance FromJSON Config
