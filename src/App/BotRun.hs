{-# LANGUAGE OverloadedStrings #-}

module App.BotRun where

import qualified App.Handlers.BotRun as Handle 
import Types.Telegram
import Realisations.Logging (addLog)
import Control.Monad.Reader
import Realisations.GettingUpdates
import Realisations.EchoMod (echoMesseging)
import Realisations.UpdatesProcessing (updateProcessing)


botRun :: Integer -> UsersSettings -> ReaderT Configurations IO ()
botRun inputOffset usersSettings = Handle.botRun handle inputOffset usersSettings
    where handle = 
            Handle.Handle { 
                    Handle.hGetUpdates = \intOffset -> Realisations.GettingUpdates.getUpdates intOffset ,
                    Handle.hAddLog = \logMsg -> Realisations.Logging.addLog logMsg ,
                    Handle.hEchoMesseging = \msg usersSettings -> Realisations.EchoMod.echoMesseging msg usersSettings ,
                    Handle.hUpdateProcessing = \fx updArr usersSettings -> Realisations.UpdatesProcessing.updateProcessing fx updArr usersSettings
                   }

