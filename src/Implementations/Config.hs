{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Implementations.Config where

import Types.Telegram
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import           Data.Foldable (asum)
import App.TelegramFromJSON
import App.TelegramToJSON


parseConfig :: IO (Maybe Config)
parseConfig = do 
    rawJSON <- BS.readFile "botConfig.json"
    let result = decodeStrict rawJSON
    return (result)