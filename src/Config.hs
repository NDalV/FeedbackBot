{-# LANGUAGE DeriveGeneric #-}

module Config where

import Data.Aeson
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)
import Subscriber

data Config = Config
  { configPort :: Int,
    configSubscribers :: [Subscriber]
  }
  deriving (Show, Generic)

fieldModifier :: String -> String
fieldModifier "configPort" = "port"
fieldModifier "configSubscribers" = "subscribers"
fieldModifier s = s

instance FromJSON Config where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = fieldModifier
        }

getConfig :: IO (Either ParseException Config)
getConfig = decodeFileEither "config.yaml"