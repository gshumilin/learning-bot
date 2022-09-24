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
    token :: Text,
    timeout :: Int,
    defaultRepeatValue :: Int,
    helpText :: Text,
    repeatText :: Text,
    unknownText :: Text
  }
  deriving (Generic, Show)

instance FromJSON Config
