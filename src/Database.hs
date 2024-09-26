{-# LANGUAGE OverloadedStrings #-}

module Database (addSubscriber, getSubscribers) where

import Control.Applicative ((<|>))
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

fileName :: FilePath
fileName = "subscribers"

addSubscriber :: Text -> IO ()
addSubscriber newSub = action <|> TIO.writeFile fileName newSub
  where
    action = do
      subscribers <- T.lines <$> TIO.readFile fileName
      case find (== newSub) subscribers of
        Nothing -> TIO.writeFile fileName $ T.unlines $ newSub : subscribers
        Just _ -> pure ()

getSubscribers :: IO [Text]
getSubscribers =
  T.lines <$> TIO.readFile fileName
    <|> (createFile >> pure [])

createFile :: IO ()
createFile = TIO.writeFile fileName ""