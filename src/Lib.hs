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

botRun :: Reader Configurations (IO ())
botRun = do
    mbUpdates <- (getUpdates offset) 
    case mbUpdates of
        Nothing -> do 
            addLog      "ERROR : getUpdates return Nothing whith this offset"
            botRun (offset + 1)
        Just updates -> do  
            if ((updatesStatus updates) == False)
                then do
                    addLog      "ERROR : In Updates ok = false"
                else do
                    let updatesArray = updatesResult updates
                    if null updatesArray
                        then do
                            addLog      "STATUS : empty update"
                            botRun offset
                        else do
                            addLog      "STATUS : start update processing"
                            updateProcessing echoMesseging updatesArray
                            botRun (extractNewOffset updatesArray)

getUpdates :: Integer -> IO (Maybe Updates)
getUpdates intOffset = do
    let token   = exampleTelegramToken
    let method  = "getUpdates"
    let offset  = BS.pack . show $ intOffset
    let request = setRequestHost        "api.telegram.org"
                $ setRequestPort        443
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestQueryString [("offset" , Just offset), ("timeout", exampleTimeout)]
                defaultRequest
    addLog      "STATUS : Sended request for updates to Telegram"   
    response <- httpBS request
    let responesBody = getResponseBody response 
    let updates = decodeStrict responesBody
    addLog $ "Got updates: \n" ++ show responesBody
    return updates      

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

sendMessage :: Message -> IO ()
sendMessage msg = do
    let token       = exampleTelegramToken
    let method      = case msg of
            StickerMessage {..}     -> "sendSticker"
            AnimationMessage {..}   -> "sendAnimation"
            _                       -> "sendMessage"
    let request = setRequestHost        exampleRequestHost
                $ setRequestPort        exampleRequestPort
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestBodyJSON    msg
                $ setRequestMethod      "POST"
                defaultRequest
    response <- httpBS request
    let logText = (BS.unpack method) ++ " to " ++ (show . chatUserName . chat $ msg) ++ ", chat_ID: " ++ (show . chatID . chat $ msg)
    addLog logText
    putStrLn logText

multipleSending :: Int -> Message -> IO ()
multipleSending 0 msg = do
    addLog      "End of multiple sending"
multipleSending n msg = do
    sendMessage msg
    multipleSending (n-1) msg

echoMesseging :: Message -> IO ()
echoMesseging msg = do
    let currChatID      = chatID . chat $ msg 
    currSettings        <- getUserSettings currChatID
    case currSettings of
        Nothing         -> do
            addLog      "STATUS : getUserSettings in echoMesseging returns Nothing"
            appendUsersSettings $ Settings  { settingsChat = chat msg
                                            , repeatValue  = defaultValueRepeat
                                            }
            multipleSending defaultValueRepeat msg
        Just settings   -> do
            let currRepeatValue = repeatValue settings
            multipleSending (repeatValue settings) msg

doCommand :: Message -> IO ()
doCommand (CommandMessage aChat aCommand) = do
    case aCommand of
        Help            -> do 
            sendMessage (TextMessage aChat exampleHelpMessage)
            addLog "Help command was done"
        Repeat          -> do 
            sendMessage (ButtonMessage aChat exampleRepeatMessageText exampleRepeatMessageKeyboard)
            addLog "Repeat command was done"
        UnknownCommand  -> do 
            sendMessage (TextMessage aChat exampleUncnownCommandMessage)
            addLog "UnknownCommand command was done"
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
appendUsersSettings settings = appendFile exampleUsersSetingsPath (show settings ++ "\n")

extractNewOffset :: [Update] -> Integer 
extractNewOffset updArray = (+1) . updateID . last $ updArray

addLog :: String -> IO ()
addLog log = do
    putStrLn log
    appendFile exampleLogPath (log ++ "\n")  

parseConfig :: IO (Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let result = decodeStrict rawJSON
    return (result)