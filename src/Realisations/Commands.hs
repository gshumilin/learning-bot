{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Realisations.Commands where

import Types.Telegram
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader
import Realisations.Logging (addLog)
import Realisations.SendingMsgs (sendMessage)
import Realisations.Settings (appendUsersSettings)
import Data.Maybe (fromJust)


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

callBackProcessing :: Callback -> UsersSettings -> ReaderT Configurations IO (UsersSettings)
callBackProcessing Callback {..} usersSettings = do
    addLog $ "STATUS : Got callback from " ++ (show . chatUserName $ callbackChat) ++ ", chat_ID: " ++ (show . chatID $ callbackChat)
    case callbackQuestion of
        "Choose your destiny"   -> do
            let newUsersSettings = appendUsersSettings (Settings { settingsChat = callbackChat, repeatValue = (read (T.unpack callbackAnswer) :: Int)}) usersSettings
            sendMessage $ TextMessage   { chat          = callbackChat
                                               , messageText   = T.pack $ "Now, I will resend you " ++ (show callbackAnswer) ++ " messeges!"
                                               }
            addLog $ "STATUS : Changet settings of " ++ (show . chatUserName $ callbackChat)
            return $ newUsersSettings 
        _                       -> do 
            addLog $ "STATUS : Unknown callback "
            return $ usersSettings