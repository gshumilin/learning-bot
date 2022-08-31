module Types.Message where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types ()
import Data.Foldable (asum)
import Data.Text (Text)
import GHC.Generics

data Message = TextMessage Text | StickerMessage Text | UnknownMessage
  deriving (Show)

instance FromJSON Message where
  parseJSON (Object o) =
    asum
      [ do
          txt <- o .: "text"
          pure $ TextMessage txt,
        do
          stickerFields <- o .: "sticker"
          fileId <- stickerFields .: "file_id"
          pure $ StickerMessage fileId,
        do
          callbackData <- o .: "data"
          pure $ TextMessage callbackData,
        pure UnknownMessage
      ]
