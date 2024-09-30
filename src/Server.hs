{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Database (getSubscribers)
import GHC.Generics (Generic)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Servant.Client (ClientEnv, runClientM)
import Telegram.Bot.API (ChatId (..), SomeChatId (..), defSendMessage, sendMessage)

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
      (sendMessage $ defSendMessage (SomeChatId $ ChatId $ read chatId) msg)
      env
  pure ()

server :: ClientEnv -> Server API
server env = distribution
  where
    distribution :: Feedback -> Handler Bool
    distribution f = do
      liftIO $ do
        subscribers <- getSubscribers
        mapM_ (sendMessageOneSubscriber env $ feedbackToMessage f) subscribers
      pure True

type API = "feedback" :> ReqBody '[JSON] Feedback :> Post '[JSON] Bool

middleware :: Middleware
middleware =
  cors $
    const $
      Just
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"],
            corsMethods = ["GET", "POST"]
          }

app :: ClientEnv -> Application
app env = middleware $ serve (Proxy :: Proxy API) $ server env

runServer :: ClientEnv -> IO ()
runServer env = putStrLn "Server started" >> run 8080 (app env)
