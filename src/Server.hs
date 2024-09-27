{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Database (getSubscribers)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp
import Servant
import Servant.Client
import Telegram.Bot.API (ChatId (..), SomeChatId (..), defSendMessage, sendMessage)

data Feedback = Feedback
  { email :: Text,
    text :: Text
  }
  deriving (Generic, Show)

instance FromJSON Feedback

sendMessageOneSubscriber :: ClientEnv -> Text -> String -> IO ()
sendMessageOneSubscriber env msg chatId = do
  _ <-
    runClientM
      (sendMessage $ defSendMessage (SomeChatId $ ChatId $ read chatId) msg)
      env
  pure ()

server :: ClientEnv -> Server API
server env maybeMsg = do
  liftIO $ do
    subscribers <- getSubscribers
    mapM_ (sendMessageOneSubscriber env $ fromMaybe "" maybeMsg) subscribers
  pure True

type API = "feedback" :> QueryParam "name" Text :> Get '[JSON] Bool

app :: ClientEnv -> Application
app env = serve (Proxy :: Proxy API) $ server env

runServer :: ClientEnv -> IO ()
runServer env = putStrLn "Server started" >> run 8080 (app env)
