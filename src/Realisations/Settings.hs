module Realisations.Telegram where

import Types.Telegram
import Data.List (find)

appendUsersSettings :: Settings -> UsersSettings -> UsersSettings
appendUsersSettings settings usersSettings = usersSettings ++ [settings]

getUserSettings :: Integer -> UsersSettings -> Maybe Settings
getUserSettings aChatID usersSettings = find (\x -> aChatID == (chatID . settingsChat $ x)) . reverse $ usersSettings
