{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Logging (
    runLoggerEnv,
    module Colog.Json,
) where

import Chronos
import Colog (cmap)
import Colog.Actions
import Colog.Json
import Colog.Json.Action
import Colog.Json.Internal.Structured
import Data.Text (Text)
import Data.Text.Internal.Builder (toLazyText)
import Data.Text.Lazy (toStrict)
import System.IO

runLoggerEnv :: (LoggerEnv -> IO a) -> IO ()
runLoggerEnv action = do
    t <- timeToDatetime <$> now
    fileHandle <- openFile "json.log" AppendMode
    hSetBuffering fileHandle LineBuffering
    let logActions = logToHandle fileHandle <> cmap messageToText logTextStdout
        context = addContext (sl "time" t) $ mkLogger logActions
    _ <- action context
    hClose fileHandle

messageToText :: Message -> Text
messageToText = (\(LogStr b) -> toStrict $ toLazyText b) . message

initLoggerEnv :: IO LoggerEnv
initLoggerEnv = do
    t <- timeToDatetime <$> now
    fileHandle <- openFile "json.log" AppendMode
    hSetBuffering fileHandle LineBuffering
    let logActions = logToHandle fileHandle <> cmap messageToText logTextStdout
        context = addContext (sl "time" t) $ mkLogger logActions
    -- hClose fileHandle -- нужно будет закрывать Handle в дальнейшем коде - как то не очень решение
    pure context
