{-# LANGUAGE OverloadedStrings #-}

module Bot where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import Database
import Servant.Client (ClientEnv)
import System.Environment (getEnv)
import Telegram.Bot.API
  ( ChatId (..),
    Token (..),
    Update,
    defaultTelegramClientEnv,
    updateChatId,
  )
import Telegram.Bot.Simple
  ( BotApp (..),
    Eff,
    replyText,
    startBot_,
    (<#),
  )
import Telegram.Bot.Simple.UpdateParser (command, parseUpdate, text)

type Model = ()

data Action
  = Start
  | SaveChatId Text
  | Message Text

echoBot :: BotApp Model Action
echoBot =
  BotApp
    { botInitialModel = (),
      botAction = flip updateToAction,
      botHandler = flip handleAction,
      botJobs = []
    }

maybeChatIdToText :: Maybe ChatId -> Text
maybeChatIdToText = maybe "" $ \(ChatId c) -> Text.pack $ show c

updateToAction :: Model -> Update -> Maybe Action
updateToAction _ update =
  parseUpdate
    ( SaveChatId (maybeChatIdToText $ updateChatId update) <$ command "start"
        <|> Message <$> text
    )
    update

handleAction :: Model -> Action -> Eff Action Model
handleAction model Start = pure model
handleAction model (SaveChatId chat) =
  model <# do
    liftIO $ do
      TIO.putStrLn $ "Chat: " <> chat
      addSubscriber chat
handleAction model (Message msg) =
  model <# do
    replyText msg

runBot :: ClientEnv -> IO ()
runBot env = do
  putStrLn "Starting Telegram bot..."
  startBot_ echoBot env