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
    { name :: Text
    , phone :: Text
    , email :: Maybe Text
    , message :: Text
    }
    deriving (Generic, Show)

instance FromJSON Feedback

feedbackToMessage :: Feedback -> Text
feedbackToMessage (Feedback n p e t) = T.unlines [n, p, fromMaybe "" e, t]

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
            print maybeOrigin -- TODO: все принты, putStrLn-ы это плохо. Есть библиотеки логов. Для начала можно посмотреть katip или co-log, они возможно уже не модные но дело свое делают. В системах эффектов идут свои встроенные библиотеки логов.
            -- по хорошему все что шлет аппликуха в stdout должно быть определенным образом форматированными json-ами, чтобы потом можно было разобрать
            -- это в каком нибудь elastic search или в куберских дашбордах по логам приложения
            let channels = sitesToChannels (fromMaybe "" maybeOrigin) subscribers
            mapM_ (sendMessageOneSubscriber env $ feedbackToMessage f) channels
        pure True

type API =
    "feedback"
        :> Header "origin" String -- TODO: почему заголовок? Причем заголовок, который зарезервирован в стандарте под другое. Лучше добавить поле в Feedback. Заодно Maybe уберешь.
        :> ReqBody '[JSON] Feedback
        :> Post '[JSON] Bool

middleware :: Middleware
middleware =
    cors $
        const $
            Just
                simpleCorsResourcePolicy
                    { corsRequestHeaders = ["Content-Type"]
                    , corsMethods = ["GET", "POST"]
                    }

-- TODO: передавать в аргументы app-у все параметры нужные для работы не очень принято.
-- в подходах основанных на реализации "в лоб", типа Handle Pattern и компания делают тип а-ля
-- data Env = Env { botEnv :: ClientEnv, config :: Config, appPort :: Int,...},  инициализируют окружение и передают на вход один агрумент а не пачку.
-- В более хитрых подходах типа эффектфул делают вообще сказочные штуки типа разбивания этого окружения на конкретные окружения.
-- Тут надо созвониться, соориентировать.
-- Ну то есть это норм, работать будет но выглядит некрасиво и тяжело масштабируется.
app :: ClientEnv -> [Subscriber] -> Application
app env = middleware . serve (Proxy :: Proxy API) . server env

runServer :: ClientEnv -> [Subscriber] -> IO ()
runServer env ss = putStrLn "Server started" >> run 8085 (app env ss) -- TODO: утащить порт в конфиг
