{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader
import           Lib

main :: IO ()
main = do 
    putStrLn "let's go"
    mbConfig <- parseConfig
    case mbConfig of
        Nothing     -> error "Config wasn't parset!"
        Just config -> do
            runReaderT (addLog "______________Telegram bot started______________") config
            runReaderT (botRun 0) config