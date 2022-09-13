import App.MessageHandling (Handle (..), HandleRes (..), UserState (..), handleMessage)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import System.IO (stdout)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Environment (Environment (..))
import Types.Log (LogDescType (..), LogLvl (..))
import Types.Message (Message (..))

main :: IO ()
main = hspec . describe "message handler test" $ do
  it "Should send help message" $ do
    let sampleSt = UserState False 2
    let sampleMsg = "/help"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (HelpMessage, UserState False 2)
  it "Should ask user for repetitions value" $ do
    let sampleSt = UserState False 2
    let sampleMsg = "/repeat"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitions, UserState True 2)
  it "Should ask user for repetitions value again, when new value isn't digit" $ do
    let sampleSt = UserState True 2
    let sampleMsg = "bad"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should ask user for repetitions value again, when new value less than zero" $ do
    let sampleSt = UserState True 2
    let sampleMsg = "-42"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should ask user for repetitions value again, when new value more than 5" $ do
    let sampleSt = UserState True 2
    let sampleMsg = "42"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should ask user for repetitions value again, when new value isn't within acceptable limits" $ do
    let sampleSt = UserState True 2
    let sampleMsg = "0"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AskForRepetitionsAgain, UserState True 2)
  it "Should successfully accept new repetitions value" $ do
    let sampleSt = UserState True 2
    let sampleMsg = "1"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (AcceptRepetitions, UserState False 1)
  it "Should successfully send echo-messages" $ do
    let sampleSt = UserState False 2
    let sampleMsg = "some_text"
    let result = runReaderT (handleMessage handle sampleSt sampleMsg) sampleConfig
    result `shouldBe` return (EchoNum 2, UserState False 2)

handle :: Handle Identity Text
handle =
  Handle
    { hSendEcho = \_ _ -> pure (),
      hAskRepetitions = \_ -> pure (),
      hSendHelpMsg = pure (),
      hSendText = \_ -> pure (),
      hGetText = Just
    }

sampleConfig :: Environment
sampleConfig =
  Environment
    { logLvl = DEBUG,
      logHandle = stdout,
      tgToken = "botToken",
      tgRequestHost = "api.telegram.org",
      tgRequestPort = 443,
      tgTimeout = 15,
      defaultRepeatValue = 1,
      helpText = "helpText",
      repeatText = "repeatText",
      unknownText = "unknownText"
    }
