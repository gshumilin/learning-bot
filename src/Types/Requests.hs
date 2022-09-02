module Types.Requests where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Text (Text)

data SendTextRequest = SendTextRequest
  { chatId :: Int,
    text :: Text
  }
  deriving (Show)

instance ToJSON SendTextRequest where
  toJSON SendTextRequest {..} =
    object
      [ "chat_id" .= chatId,
        "text" .= text
      ]

data SendStickerRequest = SendStickerRequest
  { chatId :: Int,
    stickerFileId :: Text
  }
  deriving (Show)

instance ToJSON SendStickerRequest where
  toJSON SendStickerRequest {..} =
    object
      [ "chat_id" .= chatId,
        "sticker" .= stickerFileId
      ]

data SendKeyboardRequest = SendKeyboardRequest
  { chatId :: Int,
    msgText :: Text,
    keyboard :: Keyboard
  }
  deriving (Show)

instance ToJSON SendKeyboardRequest where
  toJSON SendKeyboardRequest {..} =
    object
      [ "chat_id" .= chatId,
        "text" .= msgText,
        "reply_markup" .= keyboard
      ]

newtype Keyboard = Keyboard
  { buttonsArray :: [Button]
  }
  deriving (Show)

instance ToJSON Keyboard where
  toJSON Keyboard {..} =
    object
      [ "inline_keyboard" .= [buttonsArray]
      ]

data Button = Button
  { labelText :: Text,
    callbackData :: Text
  }
  deriving (Show)

instance ToJSON Button where
  toJSON Button {..} =
    object
      [ "text" .= labelText,
        "callback_data" .= callbackData
      ]
