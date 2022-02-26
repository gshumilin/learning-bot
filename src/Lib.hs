{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lib where

import           Control.Monad.Reader
import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           System.IO
import           System.Directory
import           Data.List
import           TelegramTypes
import           TelegramFromJSON
import           TelegramToJSON
import           Control.Monad
import           Data.Foldable (asum)
import           UsersSettings

botRun :: Integer -> IO ()
botRun inputOffset = do
    mbUpdates <- (getUpdates inputOffset) 
    case mbUpdates of
        Nothing -> do 
            addLog      "ERROR : getUpdates return Nothing whith this offset"
            botRun (inputOffset + 1)
        Just updates -> do  
            if ((updatesStatus updates) == False)
                then do
                    addLog      "ERROR : In Updates ok = false"
                else do
                    let updatesArray = updatesResult updates
                    if null updatesArray
                        then do
                            addLog      "STATUS : empty update"
                            botRun inputOffset
                        else do
                            addLog      "STATUS : start update processing"
                            updateProcessing echoMesseging updatesArray
                            botRun (extractNewOffset updatesArray)

getUpdates :: Integer -> IO (Maybe Updates)
getUpdates intOffset = runReaderT (  do
    host' <- asks confRequestHost
    let host = T.encodeUtf8 $ host'
    port <- asks confRequestPort
    token' <- asks confToken
    let token = T.encodeUtf8 $ token'
    let method = "getUpdates"
    let offset = BS.pack . show $ intOffset
    timeout' <- asks confTimeout
    let timeout = BS.pack . show $ timeout'
    let request = setRequestHost        host
                $ setRequestPort        port
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestQueryString [("offset" , Just offset), ("timeout", Just timeout)]
                defaultRequest
    response <- httpBS request
    lift $ addLog "STATUS : Sended request for updates to Telegram"   
    let responesBody = getResponseBody response 
    let updates = decodeStrict responesBody
    lift $ addLog $ "Got updates: \n" ++ show responesBody
    return updates                   
            ) xs 

updateProcessing :: (Message -> IO ()) -> [Update] -> IO ()
updateProcessing fx []          = putStrLn "End of upd"
updateProcessing fx (x : xs)    = 
    case x of
    CallbackUpdate {..} -> do 
        callBackProcessing callback
        updateProcessing fx xs
    MessageUpdate {..}  -> do
        putStrLn $ show message
        case message of
            CommandMessage {..} -> doCommand message
            _                   -> fx message
        updateProcessing fx xs
  
echoMesseging :: Message -> IO ()
echoMesseging msg = runReaderT (do
    let currChatID      = chatID . chat $ msg 
    currSettings        <- lift $ getUserSettings currChatID
    case currSettings of
        Nothing         -> do
            defaultValueRepeat <- asks confDefaultRepeatValue
            lift $ addLog      "STATUS : getUserSettings in echoMesseging returns Nothing"
            lift $ appendUsersSettings $ Settings   { settingsChat = chat msg
                                                    , repeatValue  = defaultValueRepeat
                                                    }
            lift $ multipleSending defaultValueRepeat msg
        Just settings   -> do
            let currRepeatValue = repeatValue settings
            lift $ multipleSending (repeatValue settings) msg
    ) xs

multipleSending :: Int -> Message -> IO ()
multipleSending 0 msg = do
    addLog      "End of multiple sending"
multipleSending n msg = do
    sendMessage msg
    multipleSending (n-1) msg

sendMessage :: Message -> IO ()
sendMessage msg = runReaderT (do
    host' <- asks confRequestHost
    let host = T.encodeUtf8 $ host'
    port <- asks confRequestPort
    token' <- asks confToken
    let token = T.encodeUtf8 $ token'
    let method      = case msg of
            StickerMessage {..}     -> "sendSticker"
            AnimationMessage {..}   -> "sendAnimation"
            _                       -> "sendMessage"
    let request = setRequestHost        host
                $ setRequestPort        port
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestBodyJSON    msg
                $ setRequestMethod      "POST"
                defaultRequest
    response <- httpBS request
    let logText = (BS.unpack method) ++ " to " ++ (show . chatUserName . chat $ msg) ++ ", chat_ID: " ++ (show . chatID . chat $ msg)
    lift $ addLog logText  
    ) xs

doCommand :: Message -> IO ()
doCommand (CommandMessage aChat aCommand) = runReaderT (do
    case aCommand of
        Help            -> do 
            helpText <- asks (serviceMessageText . helpCommand . confCommandMessages)
            lift $ sendMessage (TextMessage aChat helpText)
            lift $ addLog "Help command was done"
        Repeat          -> do 
            repeatText <- asks (serviceMessageText . repeatCommand . confCommandMessages)
            repeatKeyboard <- asks (serviceMessageKeyboard . repeatCommand . confCommandMessages)
            lift $ sendMessage (ButtonMessage aChat repeatText (fromJust repeatKeyboard))
            lift $ addLog "Repeat command was done"
        UnknownCommand  -> do 
            unknownText <- asks (serviceMessageText . unknownCommand . confCommandMessages)
            lift $ sendMessage (TextMessage aChat unknownText)
            lift $ addLog "UnknownCommand command was done"
    ) xs
doCommand _ = do 
    addLog "somehow doCommand was vizvana with not command message"

callBackProcessing :: Callback -> IO ()
callBackProcessing Callback {..} = do
    case callbackQuestion of
        "Choose your destiny" -> appendUsersSettings $ Settings { settingsChat = callbackChat
                                                                , repeatValue = (read (T.unpack callbackAnswer) :: Int)
                                                                }
    addLog      $ "STATUS : Got callback from " ++ (show . chatUserName $ callbackChat) ++ ", chat_ID: " ++ (show . chatID $ callbackChat)
    sendMessage $ TextMessage   { chat          = callbackChat
                                , messageText   = T.pack $ "Now, I will resend you " ++ (show callbackAnswer) ++ " messeges!"
                                }

appendUsersSettings :: Settings -> IO ()
appendUsersSettings settings = runReaderT (do 
    settingsPath' <- asks confSettingsPath
    let settingsPath = T.unpack settingsPath'
    lift $ appendFile settingsPath (show settings ++ "\n")
    ) xs

extractNewOffset :: [Update] -> Integer 
extractNewOffset updArray = (+1) . updateID . last $ updArray

addLog :: String -> IO ()
addLog log = runReaderT (do 
    logPath' <- asks confLogPath
    let logPath = T.unpack logPath'
    lift $ putStrLn log
    lift $ appendFile logPath (log ++ "\n")
    ) xs  

parseConfig :: IO (Maybe Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let result = decodeStrict rawJSON
    return (result)