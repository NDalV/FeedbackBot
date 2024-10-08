{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

-- import Config
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client (ClientEnv, runClientM)
import Subscriber hiding (channels)
import Telegram.Bot.API (SomeChatId (..), defSendMessage, sendMessage)

data Feedback = Feedback
  { email :: Text,
    message :: Text
  }
  deriving (Generic, Show)

instance FromJSON Feedback

feedbackToMessage :: Feedback -> Text
feedbackToMessage (Feedback e t) = T.unlines [e, t]

sendMessageOneSubscriber :: ClientEnv -> Text -> String -> IO ()
sendMessageOneSubscriber env msg chatId = do
  _ <-
    runClientM
      (sendMessage $ defSendMessage (SomeChatUsername $ T.pack chatId) msg)
      env
  pure ()

server :: ClientEnv -> [Subscriber] -> Server API
server env subscribers = distribution
  where
    distribution :: Maybe String -> Feedback -> Handler Bool
    distribution maybeOrigin f = do
      liftIO $ do
        print maybeOrigin
        let channels = sitesToChannels (fromMaybe "" maybeOrigin) subscribers
        mapM_ (sendMessageOneSubscriber env $ feedbackToMessage f) channels
      pure True

type API =
  "feedback"
    :> Header "origin" String
    :> ReqBody '[JSON] Feedback
    :> Post '[JSON] Bool

middleware :: Middleware
middleware =
  cors $
    const $
      Just
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"],
            corsMethods = ["GET", "POST"]
          }

app :: ClientEnv -> [Subscriber] -> Application
app env = middleware . serve (Proxy :: Proxy API) . server env

runServer :: ClientEnv -> [Subscriber] -> IO ()
runServer env ss = putStrLn "Server started" >> run 8080 (app env ss)
