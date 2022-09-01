module Implementations.Config where

import Control.Exception (IOException, catch)
import Data.Aeson (decodeStrict)
import qualified Data.ByteString.Char8 as BS (readFile)
import Types.Config (Config (..))

parseConfig :: IO (Maybe Config)
parseConfig = do
  rawJSON <-
    catch
      (BS.readFile "botConfig.json")
      ( \e -> do
          let err = show (e :: IOException)
          putStrLn $ "!!! Warning: Couldn't open: " ++ err
          return ""
      )
  let result = decodeStrict rawJSON
  return result
