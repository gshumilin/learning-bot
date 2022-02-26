{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import           Control.Monad.Reader
import           Network.HTTP.Simple
import qualified Data.ByteString.Char8 as BS
import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Text.Encoding as T
import           System.IO
import           System.Directory
import           Data.List
import           TelegramTypes
import           TelegramFromJSON
import           TelegramToJSON
import           Control.Monad
import           Data.Foldable (asum)

parseConfig :: IO (Maybe Configurations)
parseConfig = do 
    rawJSON <- BS.readFile "/home/shoomaker/Haskell/myProjects/learning-bot/src/botConfig.json"
    let res = decodeStrict rawJSON
    return (res)


------------------------------------------------------------------

main' :: IO ()
main' = do
    putStr "Hello! Enter timeout : "
    t <- getLine
    let inputTimeout = read t :: Int
    case (runReader (fx inputTimeout) xs) of
        True  -> putStrLn "Well, you are right!"
        False -> putStrLn "No, sorry :("

fx :: Int -> Reader Configurations Bool
fx x = do
    realTimeout <- asks confTimeout
    let res = x == realTimeout
    return (res)

addLog :: String -> IO ()
addLog log = do
    putStrLn log
    appendFile "/home/shoomaker/Haskell/myProjects/learning-bot/Logs.txt" (log ++ "\n") 

