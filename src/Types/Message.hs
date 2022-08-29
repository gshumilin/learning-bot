{-# LANGUAGE DeriveGeneric #-}

module Types.Message where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:))
import Data.Aeson.Types ()
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Foldable (asum)
import GHC.Generics

data Message = TextMessage Text | StickerMessage Text | MessageWithKeyboard Text KeyboardMarkup
  deriving (Show)

instance FromJSON Message where
    parseJSON (Object o) = asum [
      do
        txt <- o .: "text"
        pure $ TextMessage txt ,
      do
        txt <- o .: "sticker"
        pure $ StickerMessage txt ,
      do
        txt <- o .: "text"
        keyboard <- o .: "inlineKeyboardMarkup"
        pure $ MessageWithKeyboard txt keyboard
      ]

data ServiceMessage = ServiceMessage
  { help :: Message
  , repeat :: Message
  , unknown :: Message
  } deriving (Generic, Show)
instance FromJSON ServiceMessage

type KeyboardMarkup = [ [KeyboardButton] ] 
    
data KeyboardButton = KeyboardButton
  { buttonText :: Text
  , callbackData :: Text
  } deriving (Show, Eq)

instance FromJSON KeyboardButton where
    parseJSON (Object btn) = do
        buttonText <- btn .: "text"
        callbackData <- btn .: "callback_data"
        return $ KeyboardButton {..}