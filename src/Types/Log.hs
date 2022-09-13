{-# LANGUAGE DeriveGeneric #-}

module Types.Log where

import Data.Aeson.Types (FromJSON)
import GHC.Generics (Generic)

-- DEBUG    — logging of all events during debugging
-- WARNING  — logging errors and warnings
-- RELEASE  — logging only total errors (for example, "there is no connection to the internet")

data LogLvl = DEBUG | WARNING | RELEASE deriving (Generic, Show, Eq, Ord)

instance FromJSON LogLvl
