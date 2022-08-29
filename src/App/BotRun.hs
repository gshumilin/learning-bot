{-# LANGUAGE OverloadedStrings #-}

module App.BotRun where

import Types.Telegram
import Control.Monad.Reader
import Implementations.GettingUpdates (getUpdates)
import Implementations.Logging (addLog)
import Implementations.UpdatesProcessing (updateProcessing)
import Implementations.EchoMod (sendEcho)
import App.MessageHandling (Handle (..))

tgBot :: Int -> UsersSettings -> ReaderT Config IO ()
tgBot inputOffset usersSettings = do
  conf <- ask
  mbUpdates <- getUpdates inputOffset 
  case mbUpdates of
    Nothing -> do 
      addLog "ERROR : couldn't parse telegram updates"
      tgBot (inputOffset + 1) usersSettings
    Just updates -> do
      if (updatesStatus updates == False)
        then addLog "ERROR : In Updates ok = false"
        else do
          let updatesArray = updatesResult updates
          if null updatesArray
            then do
              addLog "STATUS : empty update"
              tgBot inputOffset usersSettings
            else do
              addLog "STATUS : start update processing"
              newUsersSettings <- updateProcessing undefined updatesArray usersSettings
              tgBot (extractNewOffset updatesArray) newUsersSettings
  -- where
  --   handle conf = Handle
  --     { hSendEcho = \msg n -> sendEcho conf msg n,
  --       hAskRepetitions = undefined,
  --       hSendText = \msg str -> sendEcho conf (messageText msg) 1,
  --       hGetText = undefined,
  --       hIsRepetitionsNum = undefined
  --     }

extractNewOffset :: [Update] -> Int
extractNewOffset updArray = (+1) . updateID . last $ updArray
