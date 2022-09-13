module Types.Message where

import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Foldable (asum)
import Data.Text (Text)

data Message = TextMessage Text | StickerMessage FileId | UnknownMessage
  deriving (Show)

type FileId = Text

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
  parseJSON _ = mzero
