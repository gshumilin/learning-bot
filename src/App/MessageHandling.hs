module App.MessageHandling where

import Data.Text (Text)
import Types.Config (Config (..))
import Types.Message (Message (..), ServiceMessages(..))
import Control.Monad.Reader (asks, ReaderT, lift)
import Prelude hiding (repeat)


data HandleRes = EchoNum Int | AskForRepetitions | AskForRepetitionsAgain | HelpMessage | AcceptRepetitions deriving (Show, Eq) 

data Handle m msg = Handle
  { hSendEcho :: msg -> Int -> m (),        -- отправить переданное сообщение переданное кол-во раз
    hAskRepetitions :: ReaderT Config m (), -- отправить пользователю repeat сообщение из конфига
    hSendHelpMsg :: ReaderT Config m (),    -- отправить пользователю help сообщение из конфига
    hSendText :: Text -> m (),              -- отправить текст
    hGetText :: msg -> Maybe Text,          -- получить текст из сообщения
    hIsRepetitionsNum :: msg -> Maybe Int   -- проверяем, что юзер прислал в качестве выбора количества повторений
  }

data UserState = UserState 
  { isAskedRepetitions :: Bool            -- находится ли юзер в процессе выбора количества повторений?
  , repetitionsNum :: Int                 -- кол-во повторений для юзера
  } deriving (Show, Eq)

handleMessage :: Monad m => Handle m msg -> UserState -> msg -> ReaderT Config m (HandleRes, UserState)
handleMessage Handle {..} st msg = do
  if isAskedRepetitions st
    then
      case hIsRepetitionsNum msg of 
        Just n -> do
          lift $ hSendText "Ok, new number of repetitions set"
          pure (AcceptRepetitions, UserState False n)
        Nothing -> do
          lift $ hSendText "Please, specify number from 1 to 5"
          pure (AskForRepetitionsAgain, st)
    else
      case hGetText msg of
        Just "/help" -> hSendHelpMsg >> pure (HelpMessage, st)
        Just "/repeat" -> hAskRepetitions >> pure (AskForRepetitions, UserState True (repetitionsNum st))
        _ -> lift $ hSendEcho msg (repetitionsNum st) >> pure (EchoNum (repetitionsNum st), st)