module Types.Telegram where

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
    =   MessageUpdate   { updateID :: Int
                        , message  :: Message
                        }
    |   CallbackUpdate  { updateID :: Int
                        , callback :: Callback
                        }
    deriving (Show, Eq)

data Chat
    =   Chat    { chatID        :: Int
                , chatUserName  :: T.Text
                } 
    deriving (Show, Read, Eq)


data Entities = Entities {entitiesType :: T.Text
                         } deriving (Show, Eq)

data Command 
    = Help | Repeat | UnknownCommand
    deriving (Show, Eq)

data Callback
    = Callback  { callbackChat      :: Chat
                , callbackQuestion  :: T.Text
                , callbackAnswer    :: T.Text
                }
    deriving (Show, Eq)

type UsersSettings = [Settings]

data Settings 
    = Settings  { settingsChat  :: Chat
                , repeatValue   :: Int
                }
    deriving (Show, Read, Eq)