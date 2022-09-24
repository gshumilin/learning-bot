{-# LANGUAGE DeriveGeneric #-}

module Types.Log where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON)
import qualified Data.Text as T (unpack)
import GHC.Generics (Generic)

-- DEBUG    — logging of all events during debugging
-- WARNING  — logging errors and warnings
-- RELEASE  — logging only total errors (for example, "there is no connection to the internet")

data LogLvl = DEBUG | WARNING | RELEASE deriving (Generic, Show, Eq, Ord)

instance FromJSON LogLvl

data LogDescType = LogFile FilePath | StdErr | StdOut deriving (Show)

instance FromJSON LogDescType where
  parseJSON (String s)
    | s == "stderr" = pure StdErr
    | s == "stdout" = pure StdOut
    | otherwise = pure . LogFile $ T.unpack s
  parseJSON _ = mzero
