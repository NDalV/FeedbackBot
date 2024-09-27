module Main where

import Bot (runBot)
import Control.Applicative ((<|>))
import Control.Concurrent.Async (Concurrently (..))
import Data.Text (pack)
import Servant.Client (ClientEnv)
import Server (runServer)
import System.Environment (getEnv)
import Telegram.Bot.API (Token (Token), defaultTelegramClientEnv)

main :: IO ()
main = do
  telegramEnv <- initClientEnv
  runConcurrently $
    Concurrently (runBot telegramEnv)
      <|> Concurrently (runServer telegramEnv)

initClientEnv :: IO ClientEnv
initClientEnv = do
  token <- Token . pack <$> getTokenStr
  defaultTelegramClientEnv token

getTokenStr :: IO String
getTokenStr =
  getEnv "FEEDBACK_BOT"
    <|> (putStrLn "Please, enter Telegram bot's API token:" >> getLine)