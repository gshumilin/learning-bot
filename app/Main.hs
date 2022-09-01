module Main where

import App.ConsoleBotRun (consoleBot)
import App.MessageHandling (UserState (..))
import App.TgBotRun (tgBot)
import Control.Monad.Reader
import Implementations.Config (parseConfig)
import Implementations.Logging (addLog)
import Types.Config
import Types.Log (LogLvl (..))

main :: IO ()
main = do
  mbConfig <- parseConfig
  case mbConfig of
    Nothing -> putStrLn "Config wasn't parsed! Bot wasn't started"
    Just config ->
      case frontEndType config of
        ConsoleFrontEnd -> do
          runReaderT (addLog RELEASE "______________Console bot started______________") config
          runReaderT (consoleBot (UserState False (defaultRepeatValue config))) config
        TelegramFrontEnd -> do
          runReaderT (addLog RELEASE "______________Telegram bot started______________") config
          runReaderT (tgBot 0 []) config
