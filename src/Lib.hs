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

botRun :: Integer -> ReaderT Configurations IO ()
botRun inputOffset = do
    mbUpdates <- getUpdates inputOffset 
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

getUpdates :: Integer -> ReaderT Configurations IO (Maybe Updates)
getUpdates intOffset = do
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
    addLog "STATUS : Sended request for updates to Telegram"   
    let responesBody = getResponseBody response 
    let updates = decodeStrict responesBody
    addLog $ "Got updates: \n" ++ show responesBody
    return updates

updateProcessing :: (Message -> ReaderT Configurations IO ()) -> [Update] -> ReaderT Configurations IO ()
updateProcessing fx []          = lift $ putStrLn "End of upd"
updateProcessing fx (x : xs)    = 
    case x of
    CallbackUpdate {..} -> do 
        callBackProcessing callback
        updateProcessing fx xs
    MessageUpdate {..}  -> do
        lift $ putStrLn $ show message
        case message of
            CommandMessage {..} -> doCommand message
            _                   -> fx message
        updateProcessing fx xs
  
echoMesseging :: Message -> ReaderT Configurations IO ()
echoMesseging msg = do
    let currChatID      = chatID . chat $ msg 
    currSettings        <- getUserSettings currChatID
    case currSettings of
        Nothing         -> do
            defaultValueRepeat <- asks confDefaultRepeatValue
            addLog      "STATUS : getUserSettings in echoMesseging returns Nothing"
            appendUsersSettings $ Settings   { settingsChat = chat msg
                                                    , repeatValue  = defaultValueRepeat
                                                    }
            multipleSending defaultValueRepeat msg
        Just settings   -> do
            let currRepeatValue = repeatValue settings
            multipleSending (repeatValue settings) msg

multipleSending :: Int -> Message -> ReaderT Configurations IO ()
multipleSending 0 msg = do
    addLog      "End of multiple sending"
multipleSending n msg = do
    sendMessage msg
    multipleSending (n-1) msg

sendMessage :: Message -> ReaderT Configurations IO ()
sendMessage msg = do
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
    addLog logText  

doCommand :: Message -> ReaderT Configurations IO ()
doCommand (CommandMessage aChat aCommand) = do
    case aCommand of
        Help            -> do 
            helpText <- asks (serviceMessageText . helpCommand . confCommandMessages)
            sendMessage (TextMessage aChat helpText)
            addLog "Help command was done"
        Repeat          -> do 
            repeatText <- asks (serviceMessageText . repeatCommand . confCommandMessages)
            repeatKeyboard <- asks (serviceMessageKeyboard . repeatCommand . confCommandMessages)
            sendMessage (ButtonMessage aChat repeatText (fromJust repeatKeyboard))
            addLog "Repeat command was done"
        UnknownCommand  -> do 
            unknownText <- asks (serviceMessageText . unknownCommand . confCommandMessages)
            sendMessage (TextMessage aChat unknownText)
            addLog "UnknownCommand command was done"
doCommand _ = do 
    addLog "somehow doCommand was vizvana with not command message"

callBackProcessing :: Callback -> ReaderT Configurations IO ()
callBackProcessing Callback {..} = do
    case callbackQuestion of
        "Choose your destiny" -> appendUsersSettings $ Settings { settingsChat = callbackChat
                                                                , repeatValue = (read (T.unpack callbackAnswer) :: Int)
                                                                }
    addLog      $ "STATUS : Got callback from " ++ (show . chatUserName $ callbackChat) ++ ", chat_ID: " ++ (show . chatID $ callbackChat)
    sendMessage $ TextMessage   { chat          = callbackChat
                                , messageText   = T.pack $ "Now, I will resend you " ++ (show callbackAnswer) ++ " messeges!"
                                }

appendUsersSettings :: Settings -> ReaderT Configurations IO ()
appendUsersSettings settings = do 
    settingsPath' <- asks confSettingsPath
    let settingsPath = T.unpack settingsPath'
    lift $ appendFile settingsPath (show settings ++ "\n")

extractNewOffset :: [Update] -> Integer 
extractNewOffset updArray = (+1) . updateID . last $ updArray

addLog :: String -> ReaderT Configurations IO ()
addLog log = do 
    logPath' <- asks confLogPath
    let logPath = T.unpack logPath'
    lift $ putStrLn log
    lift $ appendFile logPath (log ++ "\n") 

usersSettings :: ReaderT Configurations IO [Settings]
usersSettings = do
    settingsPath' <- asks confSettingsPath
    let settingsPath = T.unpack settingsPath'
    str <- lift $ readFile settingsPath
    let strList = filter (not . null) (lines str)
    let settingsList = map (\line -> read line :: Settings) strList
    return settingsList


getUserSettings :: Integer -> ReaderT Configurations IO (Maybe Settings)
getUserSettings aChatID = do
    allSettings <- usersSettings
    let res = find (\x -> aChatID == (chatID . settingsChat $ x)) . reverse $ allSettings
    return res

parseConfig :: IO (Maybe Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let result = decodeStrict rawJSON
    return (result)