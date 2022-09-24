module Implementations.Logging where

import Control.Monad.Reader (ReaderT (), ask, lift)
import qualified Data.Text as T (Text)
import Data.Text.IO (hPutStrLn)
import System.IO (Handle, IOMode (AppendMode), openFile, stderr, stdout)
import Types.Environment (Environment (..))
import Types.Log (LogDescType (..), LogLvl (..))

addLog :: LogLvl -> T.Text -> ReaderT Environment IO ()
addLog lvl logMsg = do
  Environment {..} <- ask
  if lvl >= logLvl
    then do
      lift $ hPutStrLn logHandle logMsg
    else pure ()

makeLogHandle :: LogDescType -> IO Handle
makeLogHandle (LogFile logFilePath) = openFile logFilePath AppendMode
makeLogHandle StdErr = pure stderr
makeLogHandle StdOut = pure stdout
