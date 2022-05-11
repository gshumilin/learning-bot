{-# LANGUAGE OverloadedStrings #-}

module Realisations.BotRun where

import Types.Telegram
import Realisations.Logging (addLog)
import Control.Monad.Reader
import Realisations.GettingUpdates
import Realisations.Logging
import Realisations.EchoMod (echoMesseging)
import Realisations.UpdatesProcessing (updateProcessing)


botRun :: Integer -> UsersSettings -> ReaderT Configurations IO ()
botRun inputOffset usersSettings = do
    mbUpdates <- getUpdates inputOffset 
    case mbUpdates of
        Nothing -> do 
            addLog      "ERROR : getUpdates return Nothing whith this offset"
            botRun (inputOffset + 1) usersSettings
        Just updates -> do  
            if ((updatesStatus updates) == False)
                then do
                    addLog      "ERROR : In Updates ok = false"
                else do
                    let updatesArray = updatesResult updates
                    if null updatesArray
                        then do
                            addLog      "STATUS : empty update"
                            botRun inputOffset usersSettings
                        else do
                            addLog      "STATUS : start update processing"
                            newUsersSettings <- updateProcessing (echoMesseging) updatesArray usersSettings
                            botRun (extractNewOffset updatesArray) newUsersSettings

extractNewOffset :: [Update] -> Integer 
extractNewOffset updArray = (+1) . updateID . last $ updArray