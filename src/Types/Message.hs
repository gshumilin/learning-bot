module Types.Message where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types ()
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Foldable (asum)
import GHC.Generics

data Message = TextMessage Text | StickerMessage Text | UnknownMessage
  deriving (Show)

instance FromJSON Message where
    parseJSON (Object o) = asum [
      do
        txt <- o .: "text"
        pure $ TextMessage txt ,
      do
        stickerFields <- o .: "sticker"
        fileId <- stickerFields .: "file_id"
        pure $ StickerMessage fileId ,
      do
        callbackData <- o .: "data"
        pure $ TextMessage callbackData ,
      pure UnknownMessage
      ]