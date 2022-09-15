module App.ConsoleBotRun where

import App.MessageHandling (Handle (..), handleMessage)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (getLine, putStrLn)
import Implementations.Logging (addLog)
import Types.Environment (Environment (..), UserState (..))
import Types.Log (LogLvl (..))
import Prelude hiding (repeat)

consoleBot :: ReaderT Environment IO ()
consoleBot = do
  msg <- lift T.getLine
  addLog DEBUG $ "Got message from console user: " <> msg
  res <- handleMessage handle msg
  addLog DEBUG $ "Message handling return result: " <> T.pack (show res)
  consoleBot
  where
    handle =
      Handle
        { hSendEcho = sendEcho,
          hAskRepetitions = askRepetitions,
          hSendHelpMsg = sendHelpMsg,
          hSendText = sendText,
          hGetText = getText,
          hReadUserState = readUserState,
          hModifyUserIsAsked = undefined,
          hModifyUserRepNum = undefined
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

readUserState :: ReaderT Environment m UserState
readUserState = undefined
