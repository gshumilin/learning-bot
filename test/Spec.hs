import App.MessageHandling
import Control.Monad.Reader (ReaderT, asks, lift, runReaderT)
import Data.Functor.Identity
import Test.Hspec
import Types.Config (Config (..))
import Types.Message

main :: IO ()
main = hspec . describe "message handler test" $ do
  it "Should send help message" $ do
    let sampleSt = UserState False 2
    let sampleMsg = TextMessage "/help"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (HelpMessage, UserState False 2)
  it "Should ask user for repetitions value" $ do
    let sampleSt = UserState False 2
    let sampleMsg = TextMessage "/repeat"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitions, UserState True 2)
  it "Should ask user for repetitions value again, when new value isn't digit" $ do
    let sampleSt = UserState True 2
    let sampleMsg = TextMessage "bad"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should ask user for repetitions value again, when new value isn't within acceptable limits" $ do
    let sampleSt = UserState True 2
    let sampleMsg = TextMessage "0"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should successfully accept new repetitions value" $ do
    let sampleSt = UserState True 2
    let sampleMsg = TextMessage "1"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AcceptRepetitions, UserState False 1)
  it "Should successfully send echo-messages" $ do
    let sampleSt = UserState False 2
    let sampleMsg = TextMessage "some_text"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (EchoNum 2, UserState False 2)

handle :: Handle Identity Message
handle =
  Handle
    { hSendEcho = \_ _ -> pure (),
      hAskRepetitions = pure (),
      hSendHelpMsg = pure (),
      hSendText = \_ -> pure (),
      hGetText = \msg ->
        case msg of
          TextMessage txt -> Just txt
          _ -> Nothing,
      hIsRepetitionsNum = \val ->
        case val of
          TextMessage "1" -> Just 1
          TextMessage "0" -> Nothing
          TextMessage "bad" -> Nothing
    }

sampleConfig :: Config
sampleConfig = undefined
