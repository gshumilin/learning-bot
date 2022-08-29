{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Implementations.SendingMsgs where

import Types.Telegram
import           App.TelegramFromJSON
import           App.TelegramToJSON
import Implementations.Logging (addLog)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader
import Network.HTTP.Simple

sendMessage :: Config -> Message -> IO ()
sendMessage conf msg = do
    let host = T.encodeUtf8 $ confRequestHost conf
    let token = T.encodeUtf8 $ confToken conf
    let method = case msg of
            StickerMessage {..}     -> "sendSticker"
            AnimationMessage {..}   -> "sendAnimation"
            _                       -> "sendMessage"
    let request = setRequestHost        host
                $ setRequestPort        (confRequestPort conf)
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestBodyJSON    msg
                $ setRequestMethod      "POST"
                defaultRequest
    response <- httpBS request
    pure ()
