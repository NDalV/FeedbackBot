module TelegramClientEnv where

import Control.Applicative (optional)
import Data.Text (pack)
import Servant.Client (ClientEnv)
import System.Environment (getEnv)
import Telegram.Bot.API (Token (Token), defaultTelegramClientEnv)

initClientEnv :: IO (Maybe ClientEnv)
initClientEnv = do
    token <- (Token . pack <$>) <$> getTokenStr
    traverse defaultTelegramClientEnv token

getTokenStr :: IO (Maybe String)
getTokenStr =
    optional (getEnv "FEEDBACK_BOT")