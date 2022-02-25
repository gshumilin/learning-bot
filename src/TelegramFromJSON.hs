{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module TelegramFromJSON where

import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Foldable (asum)
import           TelegramTypes

instance FromJSON Updates where
    parseJSON (Object upds) = do
        updatesStatus <- upds .: "ok"
        updatesResult <- upds .: "result"
        return $ Updates{..}

instance FromJSON Update where
    parseJSON (Object upd) = asum [
        do 
            updateID    <- upd .: "update_id"
            message     <- upd .: "message"
            return $ MessageUpdate {..} ,
        do 
            updateID    <- upd .: "update_id"
            callback    <- upd .: "callback_query"    
            return $ CallbackUpdate {..}    
        ]

instance FromJSON Message where
    parseJSON (Object msg) = asum   [
        do
            aChat            <- msg .: "chat"
            aText            <- msg .: "text" :: Parser String
            case aText of
                ('/':"help")    -> return $ CommandMessage aChat Help
                ('/':"repeat")  -> return $ CommandMessage aChat Repeat
                ('/':_)         -> return $ CommandMessage aChat UnknownCommand
                _               -> return $ TextMessage aChat (T.pack aText)  ,
        do
            chat             <- msg .: "chat"
            stickerField     <- msg .: "sticker"
            messageStickerID <- stickerField .: "file_id"
            return $ StickerMessage {..},
        do
            chat                <- msg .: "chat"
            animationField      <- msg .: "animation"
            messageAnimationID  <- animationField .: "file_id"
            return $ AnimationMessage {..}
                                    ]

instance FromJSON Chat where
    parseJSON (Object cht) = do
        chatID          <- cht .: "id"
        chatUserName    <- cht .: "username"
        return $ Chat {..}

instance FromJSON InlineKeyboardMarkup where
    parseJSON (Object mrp) = do
        inlineKeyboardMarkup      <- mrp .: "repeatInlineKeyboardMarkup"
        return $ InlineKeyboardMarkup {..}

instance FromJSON InlineKeyboardButton where
    parseJSON (Object btn) = do
        buttonText      <- btn .: "text"
        callbackData    <- btn .: "callback_data"
        return $ InlineKeyboardButton {..}

instance FromJSON Callback where
    parseJSON (Object clb) = do
        msg              <- clb .: "message"
        callbackChat     <- clb .: "from"
        callbackQuestion <- msg .: "text"
        callbackAnswer   <- clb .: "data"
        return $ Callback {..}


instance FromJSON Configurations where
    parseJSON (Object cfg) = do
        token                <- cfg .: "token"
        requestHost          <- cfg .: "requestHost"
        requestPort          <- cfg .: "requestPort"
        timeout              <- cfg .: "timeout"
        logPath              <- cfg .: "logPath"
        settingsPath         <- cfg .: "settingsPath"
        defaultRepeatValue   <- cfg .: "defaultRepeatValue"
        commandMessagesText  <- cfg .: "commandMessagesText"
        repeatKeyboardMarkup <- cfg .: "repeatKeyboardMarkup"
        return $ Configurations {..}       

instance FromJSON CommandMessagesText where
    parseJSON (Object txt) = do
        help    <- txt .: "help"
        repeat  <- txt .: "repeat"
        unknown <- txt .: "unknown"
        return $ CommandMessagesText {..}     

    
        