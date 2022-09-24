module Types.Environment where

import Data.IORef (IORef)
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
    unknownText :: Text,
    usersState :: IORef [UserState]
  }

data UserState = UserState
  { userId :: Int,
    isAskedRepetitions :: Bool, -- is user choosing the number of repetitions?
    repetitionsNum :: Int -- current number of repetitions for user
  }
  deriving (Show, Eq)
