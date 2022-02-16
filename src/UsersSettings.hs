{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module UsersSettings where

import           TelegramTypes
import qualified Data.Text as T
import           Data.List
import           BotConfig

usersSettings :: IO [Settings]
usersSettings = do
    str <- readFile exampleUsersSetingsPath
    let strList = filter (not . null) (lines str)
    let settingsList = map (\line -> read line :: Settings) strList
    return settingsList

getUserSettings :: Integer -> IO (Maybe Settings)
getUserSettings aChatID = do
    allSettings <- usersSettings
    let res = find (\x -> aChatID == (chatID . settingsChat $ x)) . reverse $ allSettings
    return res