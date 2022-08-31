module Implementations.Config where

import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS (readFile)
import Types.Config

parseConfig :: IO (Maybe Config)
parseConfig = do
  rawJSON <- BS.readFile "botConfig.json"
  let result = decodeStrict rawJSON
  return result
