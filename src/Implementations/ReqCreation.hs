module Implementations.ReqCreation where

import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T (encodeUtf8)
import Network.HTTP.Simple (Request, defaultRequest, setRequestBodyJSON, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestSecure)
import Types.Environment (Environment (..))

makeTgJsonRequest :: ToJSON a => Environment -> BS.ByteString -> a -> Request
makeTgJsonRequest env method jsonBody =
  setRequestHost "api.telegram.org" $
    setRequestPort 443 $
      setRequestSecure True $
        setRequestPath ("/bot" <> T.encodeUtf8 (token env) <> "/" <> method) $
          setRequestBodyJSON jsonBody $
            setRequestMethod
              "POST"
              defaultRequest
