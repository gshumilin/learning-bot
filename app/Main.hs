module Main where

import App.ConsoleBotRun (consoleBot)
import App.MessageHandling (UserState (..))
import App.TgBotRun (tgBot)
import Control.Monad.Reader
import qualified Data.Text as T (toLower)
import Implementations.Config (parseConfig)
import Implementations.Logging (addLog)
import Types.Config (Config (..))
import Types.Log (LogLvl (..))

main :: IO ()
main = do
  mbConfig <- parseConfig
  case mbConfig of
    Nothing -> putStrLn "Config wasn't parsed! Bot wasn't started"
    Just config ->
      case T.toLower $ frontEndType config of
        "console" ->
          runReaderT
            ( do
                addLog RELEASE "______________Console bot started______________"
                consoleBot (UserState False (defaultRepeatValue config))
            )
            config
        "telegram" ->
          runReaderT
            ( do
                addLog RELEASE "______________Telegram bot started______________"
                tgBot 0 []
            )
            config
        _ -> do
          runReaderT (addLog WARNING "Config wasn't parsed! Unknown frontend type specified. Bot wasn't started") config
