module Implementations.Logging where

import Types.Config (Config(..))
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader

addLog :: String -> ReaderT Config IO ()
addLog log = do 
    logPath' <- asks logPath
    let logPath = T.unpack logPath'
    lift $ putStrLn log
    lift $ appendFile logPath (log ++ "\n") 