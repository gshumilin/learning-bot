module App.ConsoleBotRun where

import App.MessageHandling
import Control.Monad.Reader
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (getLine, putStrLn)
import Implementations.Logging (addLog)
import Text.Read (readMaybe)
import Types.Config (Config (..))
import Types.Log (LogLvl (..))
import Types.Message (Message (..))
import Prelude hiding (repeat)

consoleBot :: UserState -> ReaderT Config IO ()
consoleBot st = do
  conf <- ask
  msg <- lift T.getLine
  let repeatVal = repetitionsNum st
  addLog DEBUG $ "Got message from console user: " <> msg
  (handleRes, newUserState) <- handleMessage (handle conf) st msg
  consoleBot newUserState
  where
    handle conf =
      Handle
        { hSendEcho = sendEcho,
          hAskRepetitions = askRepetitions,
          hSendHelpMsg = sendHelpMsg,
          hSendText = sendText,
          hGetText = getText,
          hIsRepetitionsNum = isRepetitionsNum
        }

sendEcho :: Text -> Int -> IO ()
sendEcho _ 0 = pure ()
sendEcho txt n = do
  sendText txt
  sendEcho txt (n - 1)

sendText :: Text -> IO ()
sendText = T.putStrLn

askRepetitions :: ReaderT Config IO ()
askRepetitions = do
  Config {..} <- ask
  addLog DEBUG "Called \\repeat command"
  lift $ sendText repeatText

sendHelpMsg :: ReaderT Config IO ()
sendHelpMsg = do
  txt <- asks helpText
  addLog DEBUG "Called \\help command"
  lift $ sendText txt

getText :: Text -> Maybe Text
getText = Just

isRepetitionsNum :: Text -> Maybe Int
isRepetitionsNum txt = isOkVal =<< mbNum
  where
    mbNum = readMaybe (unpack txt) :: Maybe Int
    isOkVal num = if num <= 0 && num >= 5 then Nothing else Just num
