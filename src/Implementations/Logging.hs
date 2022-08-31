module Implementations.Logging where

import Control.Monad.Reader
import qualified Data.Text as T
import Data.Text.Encoding as T
import Types.Config (Config (..))

addLog :: String -> ReaderT Config IO ()
addLog log = do
  logPath' <- asks logPath
  let logPath = T.unpack logPath'
  lift $ putStrLn log
  lift $ appendFile logPath (log ++ "\n")
