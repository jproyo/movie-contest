{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model where

import           Control.Applicative (empty)
import           Control.Lens
import           Data.Aeson          (FromJSON (..), Value (..), (.:))
import           Data.Aeson.Types

data Movie = Movie {
      _title :: String
    , _plot  :: String
    , _year  :: Integer }
    deriving (Show, Eq, Ord)

makeLenses ''Movie

instance FromJSON Movie where
    parseJSON (Object v) = do
        _title <- v .: "Title"
        _year <- read <$> v .: "Year" :: Parser Integer
        _plot <- v .: "Plot"
        return Movie{..}
    parseJSON _          = empty
