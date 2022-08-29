module App.ConsoleBotRun where

import Control.Monad.Reader 
import App.MessageHandling
import Types.Config (Config(..))
import Types.Message (Message(..), ServiceMessages (..))
import Text.Read (readMaybe)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (putStrLn, getLine)
import Prelude hiding (repeat)

consoleBot :: UserState -> ReaderT Config IO ()
consoleBot st = do
  conf <- ask
  txt <- lift $ T.getLine
  let msg = TextMessage txt
  let repeatVal = repetitionsNum st
  (handleRes, newUserState) <- handleMessage (handle conf) st msg
  consoleBot newUserState
  where
    handle conf = Handle
      { hSendEcho = \msg n -> sendEcho msg n,
        hAskRepetitions = askRepetitions,
        hSendHelpMsg = sendHelpMsg,
        hSendText = \txt -> sendText txt,
        hGetText = getText,
        hIsRepetitionsNum = isRepetitionsNum
      }

sendEcho :: Message -> Int -> IO ()
sendEcho _ 0 = pure ()
sendEcho (TextMessage txt) n = do
  sendText txt
  sendEcho (TextMessage txt) (n-1) 

sendText :: Text -> IO ()
sendText txt = T.putStrLn txt 

askRepetitions :: ReaderT Config IO ()
askRepetitions = do
  TextMessage txt <- asks (repeat . consoleServiceMessages)
  lift $ sendText txt

sendHelpMsg :: ReaderT Config IO ()
sendHelpMsg = do
  TextMessage txt <- asks (repeat . consoleServiceMessages)
  lift $ sendText txt

getText :: Message -> Maybe Text
getText (TextMessage txt) = Just txt

isRepetitionsNum :: Message -> Maybe Int
isRepetitionsNum (TextMessage txt) = isOkVal =<< mbNum
  where
    mbNum = readMaybe (unpack txt) :: Maybe Int 
    isOkVal num = if num <= 0 && num >= 5 then Nothing else Just num