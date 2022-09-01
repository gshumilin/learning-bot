module Types.Log where

import Control.Monad (mzero)
import Data.Aeson.Types (FromJSON, Value (..), parseJSON)
import Data.Text (toUpper)

-- DEBUG    — логирование всех событий при отладке.
-- WARNING  — логирование ошибок и предупреждений.
-- RELEASE  — логироване только тотальных ошибок (например "нет связи с базой данных")

data LogLvl = DEBUG | WARNING | RELEASE deriving (Show, Eq, Ord)

instance FromJSON LogLvl where
  parseJSON (String txt)
    | toUpper txt == "DEBUG" = pure DEBUG
    | toUpper txt == "WARNING" = pure WARNING
    | toUpper txt == "RELEASE" = pure RELEASE
    | otherwise = mzero
  parseJSON _ = mzero
