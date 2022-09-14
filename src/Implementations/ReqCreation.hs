module Implementations.ReqCreation where

import Data.Aeson.Types (ToJSON)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as T (encodeUtf8)
import Network.HTTP.Simple (Request, defaultRequest, setRequestBodyJSON, setRequestHost, setRequestMethod, setRequestPath, setRequestPort, setRequestSecure)
import Types.Config (Config (..))

makeTgJsonRequest :: ToJSON a => Config -> BS.ByteString -> a -> Request
makeTgJsonRequest conf method jsonBody =
  setRequestHost (T.encodeUtf8 (tgRequestHost conf)) $
    setRequestPort (tgRequestPort conf) $
      setRequestSecure True $
        setRequestPath ("/bot" <> T.encodeUtf8 (tgToken conf) <> "/" <> method) $
          setRequestBodyJSON jsonBody $
            setRequestMethod
              "POST"
              defaultRequest
