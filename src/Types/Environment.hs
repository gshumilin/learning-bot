module Types.Environment where

import Data.Text (Text)
import System.IO (Handle)
import Types.Log (LogLvl (..))

data Environment = Environment
  { logLvl :: LogLvl,
    logHandle :: Handle,
    tgToken :: Text,
    tgRequestHost :: Text,
    tgRequestPort :: Int,
    tgTimeout :: Int,
    defaultRepeatValue :: Int,
    helpText :: Text,
    repeatText :: Text,
    unknownText :: Text,
    usersState :: [UserState]
  }

data UserState = UserState
  { isAskedRepetitions :: Bool, -- is user choosing the number of repetitions?
    repetitionsNum :: Int -- current number of repetitions for user
  }
  deriving (Show, Eq)
