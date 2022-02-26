{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UsersSettings where

import           Control.Monad.Reader
import           TelegramTypes
import qualified Data.Text as T
import           Data.List

usersSettings :: IO [Settings]
usersSettings = runReaderT (do
    settingsPath' <- asks confSettingsPath
    let settingsPath = T.unpack settingsPath'
    str <- lift $ readFile settingsPath
    let strList = filter (not . null) (lines str)
    let settingsList = map (\line -> read line :: Settings) strList
    return settingsList
    ) xs

getUserSettings :: Integer -> IO (Maybe Settings)
getUserSettings aChatID = do
    allSettings <- usersSettings
    let res = find (\x -> aChatID == (chatID . settingsChat $ x)) . reverse $ allSettings
    return res

xs = Configurations {confToken = "bot2141433601:AAGWNJ-inwb_KpjD9KcWnQ_kZl1pLcmNZTw", confRequestHost = "api.telegram.org", confRequestPort = 443, confTimeout = 60, confLogPath = "/home/shoomaker/Haskell/myProjects/learning-bot/Logs.txt", confSettingsPath = "/home/shoomaker/Haskell/myProjects/learning-bot/src/usersSettingsText.txt", confDefaultRepeatValue = 1, confCommandMessages = ConfCommandMessages {helpCommand = ServiceMessage {serviceMessageText = "Send me a message, and I resend this message to you.\nSend me \"/repeat\" to chose number of repetitions", serviceMessageKeyboard = Nothing}, repeatCommand = ServiceMessage {serviceMessageText = "Choose your destiny", serviceMessageKeyboard = Just (InlineKeyboardMarkup {inlineKeyboard = [[InlineKeyboardButton {buttonText = "1", callbackData = "1"}],[InlineKeyboardButton {buttonText = "2", callbackData = "2"}],[InlineKeyboardButton {buttonText = "3", callbackData = "3"}]]})}, unknownCommand = ServiceMessage {serviceMessageText = "I dont know this command. Say, what?", serviceMessageKeyboard = Nothing}}}
