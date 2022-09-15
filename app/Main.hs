module Main where

import App.ConsoleBotRun (consoleBot)
import App.MessageHandling (UserState (..))
import App.TgBotRun (tgBot)
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef)
import qualified Data.Text as T (toLower)
import Implementations.Config (parseConfig)
import Implementations.Logging (addLog, makeLogHandle)
import System.IO (hClose)
import Types.Config (Config (..))
import qualified Types.Environment as Env (Environment (..))
import Types.Log (LogDescType (..), LogLvl (..))

main :: IO ()
main = do
  mbConfig <- parseConfig
  case mbConfig of
    Nothing -> putStrLn "Config wasn't parsed! Bot wasn't started"
    Just config -> do
      env <- makeEnvironment config
      case T.toLower $ frontEndType config of
        "console" ->
          runReaderT
            ( do
                addLog RELEASE "______________Console bot started______________"
                consoleBot (UserState False (defaultRepeatValue config))
            )
            env
        "telegram" ->
          runReaderT
            ( do
                addLog RELEASE "______________Telegram bot started______________"
                tgBot 0 []
            )
            env
        _ -> do
          runReaderT (addLog RELEASE "Config wasn't parsed! Unknown frontend type specified. Bot wasn't started") env
      case logDescType config of
        LogFile _ -> hClose $ Env.logHandle env
        _ -> pure ()

makeEnvironment :: Config -> IO Environment
makeEnvironment Config {..} = do
  logH <- makeLogHandle logDescType
  st <- newIORef []
  pure $
    Environment
      { logHandle = logH,
        usersState = st,
        ..
      }
