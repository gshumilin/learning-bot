module App.TgBotRun where

import App.MessageHandling
import Control.Monad (mapM)
import Control.Monad.Reader
import Data.Aeson (decodeStrict, encode)
import qualified Data.ByteString.Char8 as BS
import Data.List (find)
import Data.Text (Text, unpack)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import Network.HTTP.Simple (defaultRequest, getResponseBody, httpBS, setRequestBodyJSON, setRequestBodyLBS, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestQueryString, setRequestSecure)
import Text.Read (readMaybe)
import Types.Config
import Types.Message
import Types.Requests (Button (..), Keyboard (..), SendKeyboardRequest (..), SendStickerRequest (..), SendTextRequest (..))
import Types.Update

--import Implementations.Logging (addLog)

tgBot :: Int -> [(Int, UserState)] -> ReaderT Config IO ()
tgBot offset statesList = do
  mbRespond <- getUpdates offset
  case mbRespond of
    Nothing -> do
      lift $ putStrLn "Invalid JSON. Skip update"
      tgBot (offset + 1) statesList
    Just UpdatesRespond {..} -> do
      if not status
        then lift $ putStrLn "Update status = False."
        else
          if null updates
            then do
              lift $ putStrLn "Update is empty"
              tgBot offset statesList
            else do
              lift $ putStrLn "Started update processing"
              newStateList <- updatesProcessing statesList updates
              tgBot (extractNewOffset updates) newStateList

updatesProcessing :: [(Int, UserState)] -> [Update] -> ReaderT Config IO [(Int, UserState)]
updatesProcessing statesList [] = pure statesList
updatesProcessing statesList (x : xs) = do
  conf <- ask
  lift . putStrLn $ "Got message from : " ++ show (updChatId x)
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
          hGetText = getText,
          hIsRepetitionsNum = isRepetitionsNum
        }

sendEcho :: Config -> Int -> Message -> Int -> IO ()
sendEcho _ someChatId _ 0 = pure ()
sendEcho conf someChatId msg n =
  case msg of
    TextMessage txt -> do
      sendText conf someChatId txt
      sendEcho conf someChatId msg (n - 1)
    StickerMessage fileId -> do
      sendSticker conf someChatId fileId
      sendEcho conf someChatId msg (n - 1)

sendText :: Config -> Int -> Text -> IO ()
sendText conf someChatId txt = do
  let jsonBody = SendTextRequest someChatId txt
  let request =
        setRequestHost (T.encodeUtf8 (tgRequestHost conf)) $
          setRequestPort (tgRequestPort conf) $
            setRequestSecure True $
              setRequestPath ("/" <> T.encodeUtf8 (tgToken conf) <> "/" <> "sendMessage") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  response <- httpBS request
  putStrLn $ "Try to send text. Got response from telegram: " ++ show response
  pure ()

askRepetitions :: Int -> ReaderT Config IO ()
askRepetitions someChatId = do
  Config {..} <- ask
  let jsonBody =
        SendKeyboardRequest
          { chatId = someChatId,
            msgText = repeatText,
            keyboard =
              Keyboard
                [ [Button "1" "1"],
                  [Button "2" "2"],
                  [Button "3" "3"],
                  [Button "4" "4"],
                  [Button "5" "5"]
                ]
          }
  let request =
        setRequestHost (T.encodeUtf8 tgRequestHost) $
          setRequestPort tgRequestPort $
            setRequestSecure True $
              setRequestPath ("/" <> T.encodeUtf8 tgToken <> "/" <> "sendMessage") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  response <- httpBS request
  lift $ putStrLn $ "asked for Repetitions. Got response from telegram: " ++ show response
  pure ()

sendHelpMsg :: Config -> Int -> ReaderT Config IO ()
sendHelpMsg conf someChatId = do
  msg <- asks helpText
  lift $ sendText conf someChatId msg

getText :: Message -> Maybe Text
getText (TextMessage txt) = Just txt
getText (StickerMessage txt) = Just txt

isRepetitionsNum :: Message -> Maybe Int
isRepetitionsNum (TextMessage txt) = isOkVal =<< mbNum
  where
    mbNum = readMaybe (unpack txt) :: Maybe Int
    isOkVal num = if num <= 0 && num >= 5 then Nothing else Just num

extractNewOffset :: [Update] -> Int
extractNewOffset = (+ 1) . updateId . last

sendSticker :: Config -> Int -> Text -> IO ()
sendSticker conf someChatId fileId = do
  let jsonBody = SendStickerRequest someChatId fileId
  let request =
        setRequestHost (T.encodeUtf8 (tgRequestHost conf)) $
          setRequestPort (tgRequestPort conf) $
            setRequestSecure True $
              setRequestPath ("/" <> T.encodeUtf8 (tgToken conf) <> "/" <> "sendSticker") $
                setRequestBodyJSON jsonBody $
                  setRequestMethod
                    "POST"
                    defaultRequest
  response <- httpBS request
  putStrLn $ "Try to send text. Got response from telegram: " ++ show response
  pure ()

getUpdates :: Int -> ReaderT Config IO (Maybe UpdatesRespond)
getUpdates intOffset = do
  host' <- asks tgRequestHost
  let host = T.encodeUtf8 host'
  port <- asks tgRequestPort
  token' <- asks tgToken
  let token = T.encodeUtf8 token'
  let method = "getUpdates"
  let offset = BS.pack . show $ intOffset
  timeout' <- asks tgTimeout
  let timeout = BS.pack . show $ timeout'
  let request =
        setRequestHost host $
          setRequestPort port $
            setRequestSecure True $
              setRequestPath ("/" <> token <> "/" <> method) $
                setRequestQueryString
                  [("offset", Just offset), ("timeout", Just timeout)]
                  defaultRequest
  response <- httpBS request
  --addLog "STATUS : Sended request for updates to Telegram"
  let responesBody = getResponseBody response
  lift $ print response
  let updates = decodeStrict responesBody
  --addLog $ "Got updates: n" ++ show responesBody
  return updates
