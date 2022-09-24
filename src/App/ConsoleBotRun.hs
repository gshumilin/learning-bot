module App.ConsoleBotRun where

import App.MessageHandling (Handle (..), handleMessage)
import Control.Monad (replicateM_)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import Data.IORef (modifyIORef, readIORef)
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
          hModifyUserIsAsked = modifyUserIsAsked,
          hModifyUserRepNum = modifyUserRepNum
        }

sendEcho :: T.Text -> Int -> IO ()
sendEcho txt n = replicateM_ n $ sendText txt

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

readUserState :: ReaderT Environment IO UserState
readUserState = do
  Environment {..} <- ask
  sts <- lift $ readIORef usersState
  case sts of
    [] -> do
      lift $
        modifyIORef
          usersState
          ( \arr -> (UserState 1 False defaultRepeatValue) : arr
          )
      pure $ UserState 1 False defaultRepeatValue
    (x : _) -> pure x

modifyUserIsAsked :: ReaderT Environment IO ()
modifyUserIsAsked = do
  Environment {..} <- ask
  lift $ modifyIORef usersState (modifyingField defaultRepeatValue)
  where
    modifyingField defaultRepeat [] = [UserState 1 False defaultRepeat]
    modifyingField _ (UserState {..} : _) = [UserState 1 (not isAskedRepetitions) repetitionsNum]

modifyUserRepNum :: Int -> ReaderT Environment IO ()
modifyUserRepNum n = do
  sts <- asks usersState
  lift $ modifyIORef sts modifyingField
  where
    modifyingField [] = [UserState 1 True n]
    modifyingField (UserState {..} : _) = [UserState 1 isAskedRepetitions n]
