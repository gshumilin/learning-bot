module App.TgBotRun where

import App.MessageHandling (Handle (..), UserState (..), handleMessage)
import Control.Exception (throwIO)
import Control.Monad.Reader (ReaderT (..), ask, asks, lift)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS (pack)
import Data.List (find)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Implementations.ErrorHandling (BotException (..))
import Implementations.Logging (addLog)
import Network.HTTP.Client.Internal (ResponseTimeout (ResponseTimeoutMicro))
import Network.HTTP.Simple (defaultRequest, getResponseBody, httpBS, setRequestBodyJSON, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestQueryString, setRequestResponseTimeout, setRequestSecure)
import Types.Environment (Environment (..))
import Types.Log (LogLvl (..))
import Types.Message (Message (..))
import Types.Requests (Button (..), Keyboard (..), SendKeyboardRequest (..), SendStickerRequest (..), SendTextRequest (..))
import Types.Update (Update (..), UpdatesRespond (..))

tgBot :: Int -> [(Int, UserState)] -> ReaderT Environment IO ()
tgBot offset statesList = do
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
            then tgBot offset statesList
            else do
              addLog WARNING "Started update processing"
              newStateList <- updatesProcessing statesList updates
              tgBot (extractNewOffset updates) newStateList

updatesProcessing :: [(Int, UserState)] -> [Update] -> ReaderT Environment IO [(Int, UserState)]
updatesProcessing statesList [] = pure statesList
updatesProcessing statesList (x : xs) = do
  conf <- ask
  addLog DEBUG $ "Got message from: " <> (T.pack . show $ updChatId x)
  case find (\s -> fst s == updChatId x) statesList of
    Nothing -> do
      repeatValue <- asks defaultRepeatValue
      let st = UserState False repeatValue
      (_, newState) <- handleMessage (handle conf (updChatId x)) st (message x)
      let newStateList = (updChatId x, newState) : statesList
      updatesProcessing newStateList xs
    Just (cId, st) -> do
      (_, newState) <- handleMessage (handle conf cId) st (message x)
      let newStateList = (cId, newState) : statesList
      updatesProcessing newStateList xs
  where
    handle conf cId =
      Handle
        { hSendEcho = sendEcho conf cId,
          hAskRepetitions = askRepetitions cId,
          hSendHelpMsg = sendHelpMsg conf cId,
          hSendText = sendText conf cId,
          hGetText = getText
        }

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
  let request =
        setRequestHost (T.encodeUtf8 (tgRequestHost conf)) $
          setRequestPort (tgRequestPort conf) $
            setRequestSecure True $
              setRequestPath ("/bot" <> T.encodeUtf8 (tgToken conf) <> "/" <> "sendMessage") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  _ <- httpBS request
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
  let request =
        setRequestHost (T.encodeUtf8 tgRequestHost) $
          setRequestPort tgRequestPort $
            setRequestSecure True $
              setRequestPath ("/bot" <> T.encodeUtf8 tgToken <> "/" <> "sendMessage") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  _ <- httpBS request
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
  let request =
        setRequestHost (T.encodeUtf8 (tgRequestHost conf)) $
          setRequestPort (tgRequestPort conf) $
            setRequestSecure True $
              setRequestPath ("/bot" <> T.encodeUtf8 (tgToken conf) <> "/" <> "sendSticker") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  _ <- httpBS request
  pure ()

getUpdates :: Int -> ReaderT Environment IO (Maybe UpdatesRespond)
getUpdates intOffset = do
  host' <- asks tgRequestHost
  let host = T.encodeUtf8 host'
  port <- asks tgRequestPort
  token' <- asks tgToken
  let token = T.encodeUtf8 token'
  let method = "getUpdates"
  let offset = BS.pack . show $ intOffset
  timeoutInt <- asks tgTimeout
  let timeout = BS.pack $ show timeoutInt
  let request =
        setRequestHost host $
          setRequestPort port $
            setRequestSecure True $
              setRequestResponseTimeout (ResponseTimeoutMicro ((timeoutInt + 1) * 1000000)) $
                setRequestPath ("/bot" <> token <> "/" <> method) $
                  setRequestQueryString
                    [("offset", Just offset), ("timeout", Just timeout)]
                    defaultRequest
  response <- httpBS request
  addLog DEBUG "Sended request for updates to Telegram"
  let responseBody = getResponseBody response
  addLog DEBUG $ "Got response " <> T.pack (show response)
  let updates = decodeStrict responseBody
  return updates
