module Config (
    Config(..),
    getConfig
) where

import           Control.Applicative ((<$>))
import           Data.Maybe          (maybe)
import           Data.Text
import           System.Environment  (lookupEnv)

newtype Config = Config { apikey :: Text }

getConfig :: IO Config
getConfig = Config <$> getKey

getKey :: IO Text
getKey = maybe "NO_API_KEY" pack <$> lookupEnv "OMDB_API_KEY"
