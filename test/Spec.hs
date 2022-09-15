import App.MessageHandling (Handle (..), HandleRes (..), handleMessage)
import Control.Monad.Reader (runReaderT)
import Data.Functor.Identity (Identity (..))
import Data.Text (Text)
import System.IO (stdout)
import Test.Hspec (describe, hspec, it, shouldBe)
import Types.Environment (Environment (..), UserState (..))
import Types.Log (LogLvl (..))

main :: IO ()
main = hspec . describe "message handler test" $ do
  it "Should send help message" $ do
    let handle = sampleHandle {hReadUserState = pure $ UserState False 2}
    let sampleMsg = "/help"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return HelpMessage
  it "Should ask user for repetitions value" $ do
    let handle = sampleHandle {hReadUserState = pure $ UserState False 2}
    let sampleMsg = "/repeat"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AskForRepetitions
  it "Should ask user for repetitions value again, when new value isn't digit" $ do
    let handle = sampleHandle
    let sampleMsg = "bad"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AskForRepetitionsAgain
  it "Should ask user for repetitions value again, when new value less than zero" $ do
    let handle = sampleHandle
    let sampleMsg = "-42"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AskForRepetitionsAgain
  it "Should ask user for repetitions value again, when new value more than 5" $ do
    let handle = sampleHandle
    let sampleMsg = "42"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AskForRepetitionsAgain
  it "Should ask user for repetitions value again, when new value isn't within acceptable limits" $ do
    let handle = sampleHandle
    let sampleMsg = "0"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AskForRepetitionsAgain
  it "Should successfully accept new repetitions value" $ do
    let handle = sampleHandle
    let sampleMsg = "1"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return AcceptRepetitions
  it "Should successfully send echo-messages" $ do
    let handle = sampleHandle {hReadUserState = pure $ UserState False 2}
    let sampleMsg = "some_text"
    let result = runReaderT (handleMessage handle sampleMsg) sampleConfig
    result `shouldBe` return (EchoNum 2)

sampleHandle :: Handle Identity Text
sampleHandle =
  Handle
    { hSendEcho = \_ _ -> pure (),
      hAskRepetitions = \_ -> pure (),
      hSendHelpMsg = pure (),
      hSendText = \_ -> pure (),
      hGetText = Just,
      hReadUserState = pure $ UserState True 2,
      hModifyUserIsAsked = pure (),
      hModifyUserRepNum = \_ -> pure ()
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
      unknownText = "unknownText",
      usersState = []
    }
