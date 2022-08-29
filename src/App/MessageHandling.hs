module App.MessageHandling where

import Data.Text (Text)
import Types.Config (Config (..))
import Control.Monad.Reader (asks, ReaderT, lift)

data HandleRes = EchoNum Int | AskForRepetitions | AskForRepetitionsAgain | HelpMessage | AcceptRepetitions

data Handle m msg = Handle
  { hSendEcho :: msg -> Int -> m (),        -- отправить переданное сообщение переданное кол-во раз
    hAskRepetitions :: ReaderT Config m (), -- отправить пользователю сообщение просьбой указать кол-во повторений
    hSendText :: Text -> m (),              -- отправить текст
    hGetText :: msg -> Maybe Text,          -- получить текст из сообщения
    hIsRepetitionsNum :: msg -> Maybe Int   -- проверяем, что юзер прислал в качестве выбора количества повторений
  }

data UserState = UserState 
  { isAskedRepetitions :: Bool            -- находится ли юзер в процессе выбора количества повторений?
  , repetitionsNum :: Int                 -- кол-во повторений для юзера
  }

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
        Just "/help" -> lift $ hSendText "some help-message" >> pure (HelpMessage, st)
        Just "/repeat" -> hAskRepetitions >> pure (AskForRepetitions, UserState True (repetitionsNum st))
        _ -> lift $ hSendEcho msg (repetitionsNum st) >> pure (EchoNum (repetitionsNum st), st)
