{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

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
import           Control.Monad
import           Data.Foldable (asum)

parseConfig :: IO (Maybe Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let res = decodeStrict rawJSON
    return (res)


------------------------------------------------------------------

main' :: IO ()
main' = do
    putStr "Hello! Enter timeout : "
    t <- getLine
    let inputTimeout = read t :: Int
    case (runReader (fx inputTimeout) xs) of
        True  -> putStrLn "Well, you are right!"
        False -> putStrLn "No, sorry :("

fx :: Int -> Reader Configurations Bool
fx x = do
    realTimeout <- asks confTimeout
    let res = x == realTimeout
    return (res)

getUpdates :: Integer -> IO (Maybe Updates)
getUpdates intOffset = do
    let request = runReader (  do
                    host' <- asks confRequestHost
                    let host = T.encodeUtf8 $ host'
                    port <- asks confRequestPort
                    token' <- asks confToken
                    let token = T.encodeUtf8 $ token'
                    let method = "getUpdates"
                    let offset = BS.pack . show $ intOffset
                    timeout' <- asks confTimeout
                    let timeout = BS.pack . show $ timeout'
                    let request' = setRequestHost        host
                                $ setRequestPort        port
                                $ setRequestSecure      True
                                $ setRequestPath        ("/" <> token <> "/" <> method)
                                $ setRequestQueryString [("offset" , Just offset), ("timeout", Just timeout)]
                                defaultRequest
                    return request'
                ) xs
    response <- httpBS request
    addLog      "STATUS : Sended request for updates to Telegram"   
    let responesBody = getResponseBody response 
    let updates = decodeStrict responesBody
    addLog $ "Got updates: \n" ++ show responesBody
    return updates     


xs = Configurations {confToken = "bot2141433601:AAGWNJ-inwb_KpjD9KcWnQ_kZl1pLcmNZTw", confRequestHost = "api.telegram.org", confRequestPort = 443, confTimeout = 60, confLogPath = "/home/shoomaker/Haskell/myProjects/learning-bot/Logs.txt", confSettingsPath = "/home/shoomaker/Haskell/myProjects/learning-bot/src/usersSettingsText.txt", confDefaultRepeatValue = 1, confCommandMessages = ConfCommandMessages {help = ServiceMessage {serviceMessageText = "Send me a message, and I resend this message to you.\nSend me \"/repeat\" to chose number of repetitions", serviceMessageKeyboard = Nothing}, repeat = ServiceMessage {serviceMessageText = "Choose your destiny", serviceMessageKeyboard = Just (InlineKeyboardMarkup {inlineKeyboard = [[InlineKeyboardButton {buttonText = "1", callbackData = "1"}],[InlineKeyboardButton {buttonText = "2", callbackData = "2"}],[InlineKeyboardButton {buttonText = "3", callbackData = "3"}]]})}, unknown = ServiceMessage {serviceMessageText = "I dont know this command. Say, what?", serviceMessageKeyboard = Nothing}}}

addLog :: String -> IO ()
addLog log = do
    putStrLn log
    appendFile "/home/shoomaker/Haskell/myProjects/learning-bot/Logs.txt" (log ++ "\n") 