module App.MessageHandling where

import Control.Monad.Reader (ReaderT, lift)
import qualified Data.Text as T (Text, unpack)
import Text.Read (readMaybe)
import Types.Environment (Environment (..), UserState (..))
import Prelude hiding (repeat)

data HandleRes = EchoNum Int | AskForRepetitions | AskForRepetitionsAgain | HelpMessage | AcceptRepetitions deriving (Show, Eq)

data Handle m msg = Handle
  { hSendEcho :: msg -> Int -> m (), -- multiple message sending
    hAskRepetitions :: UserState -> ReaderT Environment m (), -- send repeat-message from config to user
    hSendHelpMsg :: ReaderT Environment m (), -- send help-message from config to user
    hSendText :: T.Text -> m (), -- send plain text to user
    hGetText :: msg -> Maybe T.Text, -- get text from message
    readUserState :: ReaderT Environment m UserState,
    hModifyUserIsAsked :: m (),
    hModifyUserRepNum :: Int -> m ()
  }

handleMessage :: Monad m => Handle m msg -> msg -> ReaderT Environment m HandleRes
handleMessage Handle {..} msg = do
  st@UserState {..} <- readUserState
  if isAskedRepetitions
    then case isRepetitionNum msg of
      Just n -> do
        lift $ hSendText "Ok, new number of repetitions set"
        _ <- lift $ hModifyUserRepNum n
        pure AcceptRepetitions
      Nothing -> do
        lift $ hSendText "Please, specify number from 1 to 5"
        pure AskForRepetitionsAgain
    else case hGetText msg of
      Just "/help" -> hSendHelpMsg >> pure HelpMessage
      Just "/repeat" -> hAskRepetitions st >> lift hModifyUserIsAsked >> pure AskForRepetitions
      _ -> do
        lift $ hSendEcho msg repetitionsNum
        pure $ EchoNum repetitionsNum
  where
    isRepetitionNum m = do
      txt <- hGetText m
      num <- readMaybe (T.unpack txt) :: Maybe Int
      if num <= 0 || num > 5
        then Nothing
        else Just num
