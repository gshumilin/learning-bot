module Main where

import Control.Monad.Reader
import Realisations.BotRun
import Realisations.Config (parseConfig)
import Realisations.Logging (addLog)

main :: IO ()
main = do 
    mbConfig <- parseConfig
    case mbConfig of
        Nothing     -> error "Config wasn't parsed!"
        Just config -> do
            runReaderT (addLog "______________Telegram bot started______________") config
            runReaderT (botRun 0 []) config

