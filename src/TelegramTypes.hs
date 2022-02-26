{-# LANGUAGE OverloadedStrings , RecordWildCards #-}

module TelegramTypes where

import           Data.Aeson
import           Data.Aeson.Types
import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import           Data.Foldable (asum)


data Updates 
    =   Updates { updatesStatus :: Bool
                , updatesResult :: [Update]
                } deriving (Show, Eq)

data Update 
    =   MessageUpdate   { updateID :: Integer
                        , message  :: Message
                        }
    |   CallbackUpdate  { updateID :: Integer
                        , callback :: Callback
                        }
    deriving (Show, Eq)

data Message 
    =   CommandMessage  { chat              :: Chat
                        , messageCommand    :: Command
                        } 
    |   TextMessage     { chat              :: Chat
                        , messageText       :: T.Text
                        }
    |   StickerMessage  { chat              :: Chat
                        , messageStickerID  :: T.Text
                        }
    |   AnimationMessage { chat               :: Chat
                         , messageAnimationID :: T.Text
                         }
    |   ButtonMessage   { chat              :: Chat
                        , messageText       :: T.Text
                        , keyboard          :: InlineKeyboardMarkup
                        }
    deriving (Show, Eq)

data Chat
    =   Chat    { chatID        :: Integer
                , chatUserName  :: T.Text
                } 
    deriving (Show, Read, Eq)


data Entities = Entities {entitiesType :: T.Text
                         } deriving (Show, Eq)

data Command 
    = Help | Repeat | UnknownCommand
    deriving (Show, Eq)

data InlineKeyboardMarkup
    = InlineKeyboardMarkup { inlineKeyboard :: [ [InlineKeyboardButton] ] }
    deriving (Show, Eq)
    
data InlineKeyboardButton 
    = InlineKeyboardButton  { buttonText    :: T.Text
                            , callbackData  :: T.Text
                            }
    deriving (Show, Eq)

data Callback
    = Callback  { callbackChat      :: Chat
                , callbackQuestion  :: T.Text
                , callbackAnswer    :: T.Text
                }
    deriving (Show, Eq)

data Settings 
    = Settings  { settingsChat  :: Chat
                , repeatValue   :: Int
                }
    deriving (Show, Read, Eq)

data Configurations
    = Configurations    { confToken :: T.Text
                        , confRequestHost :: T.Text
                        , confRequestPort :: Int
                        , confTimeout :: Int
                        , confLogPath :: T.Text
                        , confSettingsPath :: T.Text
                        , confDefaultRepeatValue :: Int
                        , confCommandMessages :: ConfCommandMessages 
                        } deriving Show

data ConfCommandMessages
    = ConfCommandMessages   { help    :: ServiceMessage
                            , repeat  :: ServiceMessage
                            , unknown :: ServiceMessage
                            } deriving Show

data ServiceMessage
    = ServiceMessage { serviceMessageText     :: T.Text
                     , serviceMessageKeyboard :: Maybe InlineKeyboardMarkup
                     } deriving Show