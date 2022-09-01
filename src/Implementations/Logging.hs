module Implementations.Logging where

import Control.Monad.Reader (ReaderT (), asks, lift)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (appendFile)
import Types.Config (Config (..))
import Types.Log (LogLvl (..))

addLog :: LogLvl -> T.Text -> ReaderT Config IO ()
addLog lvl logMsg = do
  currLogLvl <- asks logLvl
  if lvl >= currLogLvl
    then do
      logPath <- asks logPath
      lift $ T.appendFile logPath (logMsg <> "\n")
    else pure ()
