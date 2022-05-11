{-# LANGUAGE OverloadedStrings #-}

module App.Handlers.BotRun where

import Types.Telegram
import Realisations.Logging (addLog)
import Control.Monad.Reader
import Realisations.GettingUpdates
import Realisations.Logging
import Realisations.EchoMod (echoMesseging)
import Realisations.UpdatesProcessing (updateProcessing)

data Handle m = Handle { 
    hGetUpdates :: Integer -> ReaderT Configurations m (Maybe Updates),
    hAddLog :: String -> ReaderT Configurations m (),
    hEchoMesseging :: Message -> UsersSettings -> ReaderT Configurations m (),
    hUpdateProcessing :: (Message -> UsersSettings -> ReaderT Configurations m ()) -> [Update] -> UsersSettings -> ReaderT Configurations m (UsersSettings)
}

botRun :: Monad m => Handle m -> Integer -> UsersSettings -> ReaderT Configurations m ()
botRun handle inputOffset usersSettings = do
    mbUpdates <- (hGetUpdates handle) inputOffset 
    case mbUpdates of
        Nothing -> do 
            (hAddLog handle) "ERROR : getUpdates return Nothing whith this offset"
            botRun handle (inputOffset + 1) usersSettings
        Just updates -> do  
            if ((updatesStatus updates) == False)
                then do
                    (hAddLog handle) "ERROR : In Updates ok = false"
                else do
                    let updatesArray = updatesResult updates
                    if null updatesArray
                        then do
                            (hAddLog handle) "STATUS : empty update"
                            botRun handle inputOffset usersSettings
                        else do
                            (hAddLog handle) "STATUS : start update processing"
                            newUsersSettings <- (hUpdateProcessing handle) (hEchoMesseging handle) updatesArray usersSettings
                            botRun handle (extractNewOffset updatesArray) newUsersSettings

extractNewOffset :: [Update] -> Integer 
extractNewOffset updArray = (+1) . updateID . last $ updArray