module Adapter (AdapterHandler(..)) where

import           Config
import           Data.Text
import           Model

newtype AdapterHandler = AdapterHandler
    { runSearch :: Config -> Text -> IO (Maybe Movie) }
