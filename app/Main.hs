module Main where

import Control.Monad.Reader
import App.ConsoleBotRun (consoleBot)
import App.MessageHandling (UserState(..))
-- import App.TgBotRun (tgBot)
import Implementations.Config (parseConfig)
import Implementations.Logging (addLog)
import Types.Config

main :: IO ()
main = do 
  mbConfig <- parseConfig
  case mbConfig of
    Nothing   -> error "Config wasn't parsed!"
    Just config -> 
      case frontEndType config of
        ConsoleFrontEnd -> do
          runReaderT (addLog "______________Console bot started______________") config
          runReaderT (consoleBot (UserState False 1)) config
        TelegramFrontEnd -> do
          runReaderT (addLog "______________Telegram bot started______________") config
          runReaderT (undefined) config

