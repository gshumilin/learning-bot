{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Control.Monad.Reader
import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           System.IO
import           System.Directory
import           Data.List
import           TelegramTypes
import           TelegramFromJSON
import           Control.Monad
import           Data.Foldable (asum)

parseConfig :: IO (Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let res = decodeStrict rawJSON
    case res of
        Just result -> return (result)
        Nothing     -> error "Config was not parsed"