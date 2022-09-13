{-# LANGUAGE DeriveGeneric #-}

module Types.Config where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import GHC.Generics
import Types.Log (LogLvl (..))

data Config = Config
  { frontEndType :: Text,
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

instance FromJSON Config
