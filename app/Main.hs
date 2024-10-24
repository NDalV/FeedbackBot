module Main where

import Config (Config (configSubscribers), getConfig)
import Logging
import Server (runServer)
import TelegramClientEnv

main :: IO ()
main = do
    runLoggerEnv $ \loggerEnv -> do
        logInfo loggerEnv "Logging works"
        ec <- getConfig
        case ec of
            Left e -> logCrit loggerEnv $ showLS e
            Right config -> do
                let subscribers = configSubscribers config
                telegramEnv <- initClientEnv
                case telegramEnv of
                    Nothing -> logCrit loggerEnv "Token not available"
                    Just env -> runServer env subscribers -- сюда бы добавить логирование но не хочется добавлять еще один аргумент
