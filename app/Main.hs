{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib
import           BotConfig


main :: IO ()
main = do 
    addLog      "______________Telegram bot started______________\n"
    let startOffset = exampleOffset
    botRun startOffset