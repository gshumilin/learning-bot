{-# LANGUAGE OverloadedStrings #-}

module BotConfig where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           TelegramTypes


exampleTelegramToken :: BS.ByteString
exampleTelegramToken = "bot<...>"

exampleRequestHost :: BS.ByteString
exampleRequestHost = "api.telegram.org"

exampleRequestPort :: Int
exampleRequestPort = 443

exampleChatID :: Maybe BS.ByteString
exampleChatID = Just "477872581"

exampleText :: Maybe BS.ByteString
exampleText = Just "test text"

exampleOffset :: Integer
exampleOffset = 807400861

exampleTimeout :: Maybe BS.ByteString
exampleTimeout = Just "15"

exampleLogPath = "/home/shoomaker/Haskell/myProjects/learning-bot/Logs.txt"
exampleUsersSetingsPath = "/home/shoomaker/Haskell/myProjects/learning-bot/src/usersSettingsText.txt"

exampleHelpMessage :: T.Text
exampleHelpMessage = "Send me a message, and I resend this message to you.\nSend me \"/repeat\" to chose number of repetitions"

exampleRepeatMessageText :: T.Text
exampleRepeatMessageText = "Choose your destiny"
exampleRepeatMessageKeyboard = InlineKeyboardMarkup { inlineKeyboard = [[InlineKeyboardButton {buttonText = "1", callbackData = "1"}], [InlineKeyboardButton {buttonText = "2", callbackData = "2"}], [InlineKeyboardButton {buttonText = "3", callbackData = "3"}]] }

exampleUncnownCommandMessage :: T.Text
exampleUncnownCommandMessage = "I dont know this command. Say, what?"

defaultValueRepeat :: Int
defaultValueRepeat = 1