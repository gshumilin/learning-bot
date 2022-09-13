module App.MessageHandling where

import Control.Monad.Reader (ReaderT, lift)
import qualified Data.Text as T (Text, unpack)
import Text.Read (readMaybe)
import Types.Config (Config (..))
import Prelude hiding (repeat)

data HandleRes = EchoNum Int | AskForRepetitions | AskForRepetitionsAgain | HelpMessage | AcceptRepetitions deriving (Show, Eq)

data Handle m msg = Handle
  { hSendEcho :: msg -> Int -> m (), -- multiple message sending
    hAskRepetitions :: UserState -> ReaderT Config m (), -- send repeat-message from config to user
    hSendHelpMsg :: ReaderT Config m (), -- send help-message from config to user
    hSendText :: T.Text -> m (), -- send plain text to user
    hGetText :: msg -> Maybe T.Text -- get text from message
  }

data UserState = UserState
  { isAskedRepetitions :: Bool, -- is user choosing the number of repetitions?
    repetitionsNum :: Int -- current number of repetitions for user
  }
  deriving (Show, Eq)

handleMessage :: Monad m => Handle m msg -> UserState -> msg -> ReaderT Config m (HandleRes, UserState)
handleMessage Handle {..} st msg = do
  if isAskedRepetitions st
    then case isRepetitionNum msg of
      Just n -> do
        lift $ hSendText "Ok, new number of repetitions set"
        pure (AcceptRepetitions, UserState False n)
      Nothing -> do
        lift $ hSendText "Please, specify number from 1 to 5"
        pure (AskForRepetitionsAgain, st)
    else case hGetText msg of
      Just "/help" -> hSendHelpMsg >> pure (HelpMessage, st)
      Just "/repeat" -> hAskRepetitions st >> pure (AskForRepetitions, UserState True (repetitionsNum st))
      _ -> lift $ hSendEcho msg (repetitionsNum st) >> pure (EchoNum (repetitionsNum st), st)
  where
    isRepetitionNum m = do
      txt <- hGetText m
      num <- readMaybe (T.unpack txt) :: Maybe Int
      if num <= 0 || num > 5
        then Nothing
        else Just num
