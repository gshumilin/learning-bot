module Main where

import Control.Monad.Reader
import App.BotRun
import Implementations.Config (parseConfig)
import Implementations.Logging (addLog)

main :: IO ()
main = do 
    mbConfig <- parseConfig
    case mbConfig of
        Nothing     -> error "Config wasn't parsed!"
        Just config -> do
            runReaderT (addLog "______________Telegram bot started______________") config
            runReaderT (tgBot 0 []) config

