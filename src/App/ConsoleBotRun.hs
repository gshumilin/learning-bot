module App.ConsoleBotRun where

import App.MessageHandling (Handle (..), UserState (..), handleMessage)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (getLine, putStrLn)
import Implementations.Logging (addLog)
import Types.Environment (Environment (..))
import Types.Log (LogLvl (..))
import Prelude hiding (repeat)

consoleBot :: UserState -> ReaderT Environment IO ()
consoleBot st = do
  msg <- lift T.getLine
  addLog DEBUG $ "Got message from console user: " <> msg
  (_, newUserState) <- handleMessage handle st msg
  consoleBot newUserState
  where
    handle =
      Handle
        { hSendEcho = sendEcho,
          hAskRepetitions = askRepetitions,
          hSendHelpMsg = sendHelpMsg,
          hSendText = sendText,
          hGetText = getText
        }

sendEcho :: T.Text -> Int -> IO ()
sendEcho _ 0 = pure ()
sendEcho txt n = do
  sendText txt
  sendEcho txt (n - 1)

sendText :: T.Text -> IO ()
sendText = T.putStrLn

askRepetitions :: UserState -> ReaderT Environment IO ()
askRepetitions UserState {..} = do
  Environment {..} <- ask
  addLog DEBUG "Called /repeat command"
  lift $ sendText $ repeatText <> ". Current repetition value: " <> T.pack (show repetitionsNum)

sendHelpMsg :: ReaderT Environment IO ()
sendHelpMsg = do
  txt <- asks helpText
  addLog DEBUG "Called /help command"
  lift $ sendText txt

getText :: T.Text -> Maybe T.Text
getText = Just
