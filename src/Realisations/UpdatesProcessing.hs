{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Realisations.UpdatesProcessing where

import Types.Telegram
import           App.TelegramFromJSON
import           App.TelegramToJSON
import Realisations.Logging (addLog)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader
import Network.HTTP.Simple
import Realisations.Commands

updateProcessing :: (Message -> UsersSettings -> ReaderT Configurations IO ()) -> [Update] -> UsersSettings -> ReaderT Configurations IO (UsersSettings)
updateProcessing fx [] usersSettings = do 
    lift $ putStrLn "End of upd"
    return (usersSettings)
updateProcessing fx (x : xs) usersSettings = 
    case x of
    CallbackUpdate {..} -> do 
        newUsersSettings <- callBackProcessing callback usersSettings
        updateProcessing fx xs newUsersSettings
    MessageUpdate {..}  -> do
        lift $ putStrLn $ show message
        case message of
            CommandMessage {..} -> doCommand message
            _                   -> fx message usersSettings
        updateProcessing fx xs usersSettings