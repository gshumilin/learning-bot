{-# LANGUAGE DeriveGeneric #-}

module Types.Message where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:), (.:?), (.!=))
import Data.Aeson.Types ()
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Foldable (asum)
import GHC.Generics

data Message = TextMessage Text | StickerMessage Text
  deriving (Show)

instance FromJSON Message where
    parseJSON = undefined

data ServiceMessage = ServiceMessage
  { serviceMessageText :: Text
  , serviceMessageKeyboard :: Maybe InlineKeyboardMarkup
  } deriving (Generic, Show)

instance FromJSON ServiceMessage where
    parseJSON (Object msg) = do
        serviceMessageText      <- msg .: "text"
        serviceMessageKeyboard  <- msg .:? "commandContent" .!= Nothing
        return $ ServiceMessage {..}   

data InlineKeyboardMarkup = InlineKeyboardMarkup 
  { inlineKeyboard :: [ [InlineKeyboardButton] ] }
    deriving (Show, Eq)

instance FromJSON InlineKeyboardMarkup where
  parseJSON (Object mrp) = do
      inlineKeyboard      <- mrp .: "inlineKeyboardMarkup"
      return $ InlineKeyboardMarkup {..}
    
data InlineKeyboardButton 
    = InlineKeyboardButton  { buttonText    :: Text
                            , callbackData  :: Text
                            }
    deriving (Show, Eq)

instance FromJSON InlineKeyboardButton where
    parseJSON (Object btn) = do
        buttonText      <- btn .: "text"
        callbackData    <- btn .: "callback_data"
        return $ InlineKeyboardButton {..}