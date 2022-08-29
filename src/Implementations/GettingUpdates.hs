module Implementations.GettingUpdates where

import Types.Telegram
import           App.TelegramFromJSON
import           App.TelegramToJSON
import Implementations.Logging (addLog)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader
import Network.HTTP.Simple
import Data.Aeson

getUpdates :: Int -> ReaderT Config IO (Maybe Updates)
getUpdates intOffset = do
    host' <- asks confRequestHost
    let host = T.encodeUtf8 $ host'
    port <- asks confRequestPort
    token' <- asks confToken
    let token = T.encodeUtf8 $ token'
    let method = "getUpdates"
    let offset = BS.pack . show $ intOffset
    timeout' <- asks confTimeout
    let timeout = BS.pack . show $ timeout'
    let request = setRequestHost        host
                $ setRequestPort        port
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestQueryString [("offset" , Just offset), ("timeout", Just timeout)]
                defaultRequest
    response <- httpBS request
    addLog "STATUS : Sended request for updates to Telegram"   
    let responesBody = getResponseBody response 
    let updates = decodeStrict responesBody
    addLog $ "Got updates: \n" ++ show responesBody
    return updates