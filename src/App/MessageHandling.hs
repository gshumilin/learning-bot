module App.MessageHandling where

data HandleRes = EchoNum Int | AskForRepetitions | AskForRepetitionsAgain | HelpMessage | AcceptRepetitions

data Handle m msg = Handle
  { hSendEcho :: msg -> Int -> m (),
    hAskRepetitions :: m (),
    hSendText :: String -> m (),
    hGetText :: msg -> Maybe String,
    hIsRepetitionsNum :: msg -> Maybe Int
  }

data UserState = UserState { isAskedRepetitions :: Bool, repetitionsNum :: Int }

handleUpdate :: Monad m => Handle m msg -> msg -> UserState -> m (HandleRes, UserState)
handleUpdate Handle {..} msg st = do
  if isAskedRepetitions st
    then
      case hIsRepetitionsNum msg of
        Just n -> do
            hSendText "Ok bob"
            pure (AcceptRepetitions, UserState False n)
        Nothing -> do
            hSendText "Please, specify number from 1 to 5"
            pure (AskForRepetitionsAgain, st)
    else
      case hGetText msg of
        Just "/help" -> hSendText "Hello, bob" >> pure (HelpMessage, st)
        Just "/repeat" -> hAskRepetitions >> pure (AskForRepetitions, UserState True (repetitionsNum st))
        _ -> hSendEcho msg (repetitionsNum st) >> pure (EchoNum (repetitionsNum st), st)
