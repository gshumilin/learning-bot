module Implementations.Logging where

import Types.Telegram
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader

addLog :: String -> ReaderT Config IO ()
addLog log = do 
    logPath' <- asks confLogPath
    let logPath = T.unpack logPath'
    lift $ putStrLn log
    lift $ appendFile logPath (log ++ "\n") 