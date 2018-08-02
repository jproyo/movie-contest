module OmdbAdapter (newHandle) where

import           Adapter
import           Config
import           Data.Aeson            (FromJSON (..))
import qualified Data.ByteString.Char8 as S8
import           Data.Either           (either)
import           Data.Text             (Text)
import qualified Data.Text.Encoding    as E
import           Model
import           Network.HTTP.Client   (setQueryString)
import           Network.HTTP.Simple

extractResponse :: FromJSON a => Request -> IO (Maybe a)
extractResponse request = do
    response <- httpJSONEither request
    return $ either (const Nothing) id (getResponseBody response)

reqParams :: Text -> Text -> [(S8.ByteString, Maybe S8.ByteString)]
reqParams key term =
    [ ("apikey", Just (E.encodeUtf8 key))
    , ("t", Just (E.encodeUtf8 term)) ]

searchTitleOmdb :: Config -> Text -> IO (Maybe Movie)
searchTitleOmdb conf term = do
    request <- parseRequest "http://www.omdbapi.com"
    let request'
            = setRequestMethod "GET"
            $ setRequestPath "/"
            $ setQueryString (reqParams (apikey conf) term) request
    extractResponse request'


newHandle :: AdapterHandler
newHandle = AdapterHandler { runSearch = searchTitleOmdb }
