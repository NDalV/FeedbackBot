{-# LANGUAGE DeriveGeneric #-}

module Subscriber where

import Data.Aeson
import Data.List
import GHC.Generics (Generic)

data Subscriber = Subscriber
    { channels :: [String]
    , sites :: [String]
    }
    deriving (Show, Generic)

instance FromJSON Subscriber

sitesToChannels :: String -> [Subscriber] -> [String]
sitesToChannels site = foldr foldFunc []
  where
    foldFunc :: Subscriber -> [String] -> [String]
    foldFunc s = case find (== site) (sites s) of
        Nothing -> id
        Just _ -> const $ channels s
