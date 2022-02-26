{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Lib

main :: IO ()
main = do 
    addLog      "______________Telegram bot started______________\n"
    botRun 0