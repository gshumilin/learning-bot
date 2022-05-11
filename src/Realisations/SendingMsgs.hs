{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Realisations.SendingMsgs where

import Types.Telegram
import           App.TelegramFromJSON
import           App.TelegramToJSON
import Realisations.Logging (addLog)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding as T
import Control.Monad.Reader
import Network.HTTP.Simple



sendMessage :: Message -> ReaderT Configurations IO ()
sendMessage msg = do
    host' <- asks confRequestHost
    let host = T.encodeUtf8 $ host'
    port <- asks confRequestPort
    token' <- asks confToken
    let token = T.encodeUtf8 $ token'
    let method      = case msg of
            StickerMessage {..}     -> "sendSticker"
            AnimationMessage {..}   -> "sendAnimation"
            _                       -> "sendMessage"
    let request = setRequestHost        host
                $ setRequestPort        port
                $ setRequestSecure      True
                $ setRequestPath        ("/" <> token <> "/" <> method)
                $ setRequestBodyJSON    msg
                $ setRequestMethod      "POST"
                defaultRequest
    response <- httpBS request
    let logText = (BS.unpack method) ++ " to " ++ (show . chatUserName . chat $ msg) ++ ", chat_ID: " ++ (show . chatID . chat $ msg)
    addLog logText  