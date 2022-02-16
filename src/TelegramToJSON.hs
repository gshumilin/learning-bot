{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module TelegramToJSON where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encode.Pretty
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Foldable (asum)
import           TelegramTypes 

instance ToJSON Message where
    toJSON TextMessage {..} =
        object  [ "chat_id"     .= chatID chat
                , "text"        .= messageText
                ] 
    toJSON StickerMessage {..} =
        object  [ "chat_id"     .= chatID chat
                , "sticker"     .= messageStickerID
                ] 
    toJSON AnimationMessage {..} =
        object  [ "chat_id"     .= chatID chat
                , "animation"   .= messageAnimationID
                ] 
    toJSON ButtonMessage {..} =
        object  [ "chat_id"         .= chatID chat
                , "text"            .= messageText
                , "reply_markup"    .= keyboard
                ] 

instance ToJSON InlineKeyboardMarkup where
    toJSON InlineKeyboardMarkup {..} =
        object  [ "inline_keyboard" .= inlineKeyboard
                ] 

instance ToJSON InlineKeyboardButton where
    toJSON InlineKeyboardButton {..} =
        object  [ "text"            .= buttonText
                , "callback_data"   .= callbackData
                ] 