module App.ConsoleBotRun where

import App.MessageHandling (Handle (..), UserState (..), handleMessage)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T (getLine, putStrLn)
import Implementations.Logging (addLog)
import Text.Read (readMaybe)
import Types.Config (Config (..))
import Types.Log (LogLvl (..))
import Prelude hiding (repeat)

consoleBot :: UserState -> ReaderT Config IO ()
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
          hGetText = getText,
          hIsRepetitionsNum = isRepetitionsNum
        }

sendEcho :: T.Text -> Int -> IO ()
sendEcho _ 0 = pure ()
sendEcho txt n = do
  sendText txt
  sendEcho txt (n - 1)

sendText :: T.Text -> IO ()
sendText = T.putStrLn

askRepetitions :: UserState -> ReaderT Config IO ()
askRepetitions UserState {..} = do
  Config {..} <- ask
  addLog DEBUG "Called \\repeat command"
  lift $ sendText $ repeatText <> "Current repetition value: " <> T.pack (show repetitionsNum)

sendHelpMsg :: ReaderT Config IO ()
sendHelpMsg = do
  txt <- asks helpText
  addLog DEBUG "Called \\help command"
  lift $ sendText txt

getText :: T.Text -> Maybe T.Text
getText = Just

isRepetitionsNum :: T.Text -> Maybe Int
isRepetitionsNum txt = isOkVal =<< mbNum
  where
    mbNum = readMaybe (T.unpack txt) :: Maybe Int
    isOkVal num = if num <= 0 && num >= 5 then Nothing else Just num
