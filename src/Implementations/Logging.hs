module Implementations.Logging where

import Control.Monad.Reader (ReaderT (), asks, lift)
import qualified Data.Text as T (Text, pack)
import qualified Data.Text.IO as T (appendFile)
import Data.Time (getCurrentTime)
import Types.Config (Config (..))
import Types.Log (LogLvl (..))

addLog :: LogLvl -> T.Text -> ReaderT Config IO ()
addLog lvl logMsg = do
  currLogLvl <- asks logLvl
  if lvl >= currLogLvl
    then do
      logPath <- asks logPath
      time <- lift getCurrentTime
      let logLevel = T.pack . show $ lvl
      lift $ T.appendFile logPath (T.pack (show time) <> " :: " <> logLevel <> " :: " <> logMsg <> "\n")
    else pure ()
