{-# LANGUAGE DeriveGeneric #-}

module Types.Requests where

import Data.Aeson (ToJSON, toJSON, Value (..), (.:))
import Control.Monad (mzero)
import Data.Text (Text)
import Data.Foldable (asum)
import GHC.Generics

data SendMessageRequest = SendMessageRequest 
  { chatId :: Int
  , text :: Message
  } deriving Show

instance ToJSON SendMessageRequest where
  toJSON = undefined
