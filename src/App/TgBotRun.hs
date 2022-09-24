module App.TgBotRun where

import App.MessageHandling (Handle (..), handleMessage)
import Control.Exception (throwIO)
import Control.Monad (void)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.IORef (modifyIORef, readIORef)
import Data.List (find)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Implementations.ErrorHandling (BotException (..))
import Implementations.Logging (addLog)
import Implementations.ReqCreation (makeTgJsonRequest)
import Network.HTTP.Client.Internal (ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (defaultRequest, getResponseBody, httpBS, setRequestHost, setRequestPath, setRequestPort, setRequestQueryString, setRequestResponseTimeout, setRequestSecure)
import Types.Config (Config (..))
import Types.Environment (Environment (..), UserState (..))
import Types.Log (LogLvl (..))
import Types.Message (Message (..))
import Types.Requests (Button (..), Keyboard (..), SendKeyboardRequest (..), SendStickerRequest (..), SendTextRequest (..))
import Types.Update (Update (..), UpdatesRespond (..))

tgBot :: Int -> ReaderT Environment IO ()
tgBot offset = do
  mbRespond <- getUpdates offset
  case mbRespond of
    Nothing -> do
      addLog WARNING "Invalid JSON."
      lift $ throwIO TelegramAPIException
    Just UpdatesRespond {..} -> do
      if not status
        then do
          addLog WARNING "Update status = False"
          lift $ throwIO TelegramAPIException
        else
          if null updates
            then tgBot offset
            else do
              addLog WARNING "Started update processing"
              updatesProcessing updates
              tgBot $ extractNewOffset updates

updatesProcessing :: [Update] -> ReaderT Environment IO ()
updatesProcessing [] = pure ()
updatesProcessing (x : xs) = do
  env <- ask
  let cId = updChatId x
  addLog DEBUG $ "Got message from: " <> (T.pack $ show cId)
  _ <- handleMessage (handle env cId) (message x)
  updatesProcessing xs
  where
    handle env cId =
      Handle
        { hSendEcho = sendEcho env cId,
          hAskRepetitions = askRepetitions cId,
          hSendHelpMsg = sendHelpMsg env cId,
          hSendText = sendText env cId,
          hGetText = getText,
          hReadUserState = readUserState cId,
          hModifyUserIsAsked = modifyUserIsAsked cId,
          hModifyUserRepNum = modifyUserRepNum cId
        }

readUserState :: Int -> ReaderT Environment IO UserState
readUserState i = do
  Environment {..} <- ask
  sts <- lift $ readIORef usersState
  case find (\UserState {..} -> userId == i) sts of
    Nothing -> do
      let defaultUserState = UserState i False defaultRepeatValue
      lift $ modifyIORef usersState (\arr -> defaultUserState : arr)
      pure defaultUserState
    Just st -> pure st

modifyUserIsAsked :: Int -> ReaderT Environment IO ()
modifyUserIsAsked i = do
  Environment {..} <- ask
  lift $ modifyIORef usersState modifyingField
  where
    modifyingField =
      foldr
        ( \s@UserState {..} acc ->
            if userId == i
              then UserState userId (not isAskedRepetitions) repetitionsNum : acc
              else s : acc
        )
        []

modifyUserRepNum :: Int -> Int -> ReaderT Environment IO ()
modifyUserRepNum i n = do
  sts <- asks usersState
  lift $ modifyIORef sts modifyingField
  where
    modifyingField =
      foldr
        ( \s@UserState {..} acc ->
            if userId == i
              then UserState userId isAskedRepetitions n : acc
              else s : acc
        )
        []

sendEcho :: Environment -> Int -> Message -> Int -> IO ()
sendEcho _ _ _ 0 = pure ()
sendEcho conf someChatId msg n =
  case msg of
    TextMessage txt -> do
      sendText conf someChatId txt
      sendEcho conf someChatId msg (n - 1)
    StickerMessage fileId -> do
      sendSticker conf someChatId fileId
      sendEcho conf someChatId msg (n - 1)
    UnknownMessage -> do
      let txt = unknownText conf
      sendText conf someChatId txt
      sendEcho conf someChatId msg (n - 1)

sendText :: Environment -> Int -> T.Text -> IO ()
sendText conf someChatId txt = do
  let jsonBody = SendTextRequest someChatId txt
  let req = makeTgJsonRequest conf "sendMessage" jsonBody
  _ <- httpBS req
  pure ()

askRepetitions :: Int -> UserState -> ReaderT Environment IO ()
askRepetitions someChatId UserState {..} = do
  Environment {..} <- ask
  let jsonBody =
        SendKeyboardRequest
          { chatId = someChatId,
            msgText = repeatText <> ". Current repetition value: " <> T.pack (show repetitionsNum),
            keyboard =
              Keyboard
                [ Button "1" "1",
                  Button "2" "2",
                  Button "3" "3",
                  Button "4" "4",
                  Button "5" "5"
                ]
          }
  let req = makeTgJsonRequest conf "sendMessage" jsonBody
  _ <- httpBS req
  pure ()

sendHelpMsg :: Environment -> Int -> ReaderT Environment IO ()
sendHelpMsg conf someChatId = do
  msg <- asks helpText
  lift $ sendText conf someChatId msg

getText :: Message -> Maybe T.Text
getText (TextMessage txt) = Just txt
getText (StickerMessage txt) = Just txt
getText UnknownMessage = Nothing

extractNewOffset :: [Update] -> Int
extractNewOffset [] = 1
extractNewOffset xxs = (\(x : _) -> (+ 1) $ updateId x) $ reverse xxs

sendSticker :: Environment -> Int -> T.Text -> IO ()
sendSticker conf someChatId fileId = do
  let jsonBody = SendStickerRequest someChatId fileId
  let req = makeTgJsonRequest conf "sendSticker" jsonBody
  _ <- httpBS req
  pure ()

getUpdates :: Int -> ReaderT Environment IO (Maybe UpdatesRespond)
getUpdates intOffset = do
  token' <- asks token
  let confToken = T.encodeUtf8 token'
  let method = "getUpdates"
  let offset = BS.pack . show $ intOffset
  timeoutInt <- asks timeout
  let timeoutText = BS.pack $ show timeoutInt
  let request =
        setRequestHost "api.telegram.org" $
          setRequestPort 443 $
            setRequestSecure True $
              setRequestResponseTimeout (ResponseTimeoutMicro ((timeoutInt + 1) * 1000000)) $
                setRequestPath ("/bot" <> confToken <> "/" <> method) $
                  setRequestQueryString
                    [("offset", Just offset), ("timeout", Just timeoutText)]
                    defaultRequest
  response <- httpBS request
  addLog DEBUG "Sended request for updates to Telegram"
  let responseBody = getResponseBody response
  addLog DEBUG $ "Got response " <> T.pack (show response)
  let updates = decodeStrict responseBody
  return updates
