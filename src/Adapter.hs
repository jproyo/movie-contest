{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Adapter where

import           Config
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Reader   (MonadReader)
import           Data.Text
import           Model

class (MonadReader Config m, MonadIO m) => MovieSearcher m where
    search :: Text -> m (Maybe Movie)
