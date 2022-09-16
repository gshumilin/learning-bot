module Types.Environment where

import Data.Text (Text)
import System.IO (Handle)
import Types.Log (LogLvl (..))

data Environment = Environment
  { logLvl :: LogLvl,
    logHandle :: Handle,
    token :: Text,
    timeout :: Int,
    defaultRepeatValue :: Int,
    helpText :: Text,
    repeatText :: Text,
    unknownText :: Text
  }
