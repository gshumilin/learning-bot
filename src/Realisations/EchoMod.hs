module Realisations.EchoMod where

import Types.Telegram
import Realisations.Logging (addLog)
import Control.Monad.Reader
import Realisations.SendingMsgs (sendMessage)
import Realisations.Settings (getUserSettings)

echoMesseging :: Message -> UsersSettings -> ReaderT Configurations IO ()
echoMesseging msg usersSettings = do
    let currChatID   = chatID . chat $ msg 
    let currSettings = getUserSettings currChatID usersSettings
    case currSettings of
        Nothing         -> do
            defaultValueRepeat <- asks confDefaultRepeatValue
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