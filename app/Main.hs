module Main where

import Config (Config (configSubscribers), getConfig)
import Control.Applicative ((<|>))
import Data.Text (pack)
import Servant.Client (ClientEnv)
import Server (runServer)
import System.Environment (getEnv)
import Telegram.Bot.API (Token (Token), defaultTelegramClientEnv)

main :: IO ()
main = do
  ec <- getConfig
  case ec of
    Left e -> print e
    Right config -> do
      let subscribers = configSubscribers config
      telegramEnv <- initClientEnv
      runServer telegramEnv subscribers

initClientEnv :: IO ClientEnv
initClientEnv = do
  token <- Token . pack <$> getTokenStr
  defaultTelegramClientEnv token

getTokenStr :: IO String
getTokenStr =
  getEnv "FEEDBACK_BOT"
    <|> (putStrLn "Please, enter Telegram bot's API token:" >> getLine)