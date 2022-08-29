{-# LANGUAGE DeriveGeneric #-}

module Types.Message where

import Data.Aeson (FromJSON, parseJSON, Value (..), (.:))
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

data InlineKeyboardMarkup
    = InlineKeyboardMarkup { inlineKeyboard :: [ [InlineKeyboardButton] ] }
    deriving (Show, Eq)
    
data InlineKeyboardButton 
    = InlineKeyboardButton  { buttonText    :: Text
                            , callbackData  :: Text
                            }
    deriving (Show, Eq)