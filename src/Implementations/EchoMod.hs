module Implementations.EchoMod where

import Types.Telegram
import Implementations.Logging (addLog)
import Control.Monad.Reader
import Implementations.SendingMsgs (sendMessage)
import Implementations.Settings (getUserSettings)

-- echoMesseging :: Message -> UsersSettings -> ReaderT Config IO ()
-- echoMesseging msg usersSettings = do
--     let currChatID   = chatID . chat $ msg 
--     let currSettings = getUserSettings currChatID usersSettings
--     case currSettings of
--         Nothing         -> do
--             defaultValueRepeat <- asks confDefaultRepeatValue
--             multipleSending defaultValueRepeat msg
--         Just settings   -> do
--             let currRepeatValue = repeatValue settings
--             multipleSending (repeatValue settings) msg

sendEcho :: Config -> Message -> Int -> IO ()
sendEcho _ _ 0 = pure ()
sendEcho conf msg n = do
    sendMessage conf msg
    sendEcho conf msg (n-1)