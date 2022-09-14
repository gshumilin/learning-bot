module Implementations.ErrorHandling where

import Control.Exception (Exception)
import Data.Typeable (Typeable)

data BotException = TelegramAPIException
  deriving (Show, Typeable)

instance Exception BotException
