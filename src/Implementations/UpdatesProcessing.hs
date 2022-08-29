{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Implementations.UpdatesProcessing where

-- import Types.Message
-- import Types.Config
-- import Implementations.Logging (addLog)
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Text as T
-- import Data.Text.Encoding as T
-- import Control.Monad.Reader
-- import Network.HTTP.Simple

-- updateProcessing :: (Message -> UsersSettings -> ReaderT Config IO ()) -> [Update] -> UsersSettings -> ReaderT Config IO (UsersSettings)
-- updateProcessing fx [] usersSettings = do 
--     lift $ putStrLn "End of upd"
--     return (usersSettings)
-- updateProcessing fx (x : xs) usersSettings = 
--     case x of
--     CallbackUpdate {..} -> do 
--         newUsersSettings <- callBackProcessing callback usersSettings
--         updateProcessing fx xs newUsersSettings
--     MessageUpdate {..}  -> do
--         lift $ putStrLn $ show message
--         case message of
--             CommandMessage {..} -> doCommand message
--             _                   -> fx message usersSettings
--         updateProcessing fx xs usersSettings