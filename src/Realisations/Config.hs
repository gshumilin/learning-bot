{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Realisations.Config where

import Types.Telegram
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
--import qualified Data.Text.Lazy.IO as T
--import qualified Data.Text.Lazy.Encoding as T
import           Data.Foldable (asum)
import App.TelegramFromJSON
import App.TelegramToJSON


parseConfig :: IO (Maybe Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "botConfig.json"
    let result = decodeStrict rawJSON
    return (result)