{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OmdbAdapter (searchTitleOmdb) where

import           Config
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Aeson             (FromJSON (..))
import qualified Data.ByteString.Char8  as S8
import           Data.Either            (either)
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as E
import           Model
import           Network.HTTP.Client    (setQueryString)
import           Network.HTTP.Simple

extractResponse :: FromJSON a => Request -> IO (Maybe a)
extractResponse request = do
    response <- httpJSONEither request
    return $ either (const Nothing) id (getResponseBody response)

reqParams :: Text -> Text -> [(S8.ByteString, Maybe S8.ByteString)]
reqParams key term =
    [ ("apikey", Just (E.encodeUtf8 key))
    , ("t", Just (E.encodeUtf8 term)) ]

searchTitleOmdb :: (MonadReader Config m, MonadIO m) => Text -> m (Maybe Movie)
searchTitleOmdb term = do
    key <- asks apikey
    liftIO $ do
        request <- parseRequest "http://www.omdbapi.com"
        let request'
                = setRequestMethod "GET"
                $ setRequestPath "/"
                $ setQueryString (reqParams key term) request
        extractResponse request'
