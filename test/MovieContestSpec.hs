module MovieContestSpec (spec) where

import           Adapter
import           Config
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State  (execStateT)
import           Data.List
import           Data.Maybe
import           Data.Text            (Text, pack)
import           Model
import           MovieContest
import           Test.Hspec

spec :: Spec
spec = describe "tryGuess using mocking for omdbapi" $ do
    it "user should win because movie not found" $ do
        conf <- getConfig
        (PlayState _ xs _)  <- execStateT
            (runReaderT (runContest $ tryGuess "some movie not found") conf)
            initialNotFound
        catMaybes xs `shouldSatisfy` ((0==) . length)
    it "movie should be stored on state because movie found" $ do
        conf <- getConfig
        (PlayState _ xs _)  <- execStateT
            (runReaderT (runContest $ tryGuess "movie e") conf)
            initialFound
        catMaybes xs `shouldSatisfy` ((1==) . length)
    it "machine should win because 3 attemps found" $ do
        conf <- getConfig
        (PlayState _ xs _)  <- execStateT
            (runReaderT (runContest $ sequence [tryGuess "movie e" , tryGuess "movie a" , tryGuess "movie c"]) conf)
            initialFound
        catMaybes xs `shouldSatisfy` ((3==) . length)
    it "user should win because 2 attemps ok and after that failed" $ do
        conf <- getConfig
        (PlayState _ xs _)  <- execStateT
            (runReaderT (runContest $ sequence [tryGuess "movie e" , tryGuess "movie a" , tryGuess "movie z"]) conf)
            initialFound
        catMaybes xs `shouldSatisfy` ((2==) . length)

initialNotFound :: PlayState
initialNotFound = initialStateWithHandler handlerMockNotFound

initialFound :: PlayState
initialFound = initialStateWithHandler handlerMockFound

handlerMockNotFound :: AdapterHandler
handlerMockNotFound = AdapterHandler { runSearch = \_ _ -> return Nothing }

handlerMockFound :: AdapterHandler
handlerMockFound = AdapterHandler { runSearch = \_ t -> return $ searchMovie t }

movies :: [Movie]
movies = [ Movie {_title = "movie a", _plot = "desc a", _year = 1997}
     , Movie {_title = "movie b", _plot = "desc b", _year = 1997}
     , Movie {_title = "movie c", _plot = "desc c", _year = 1997}
     , Movie {_title = "movie d", _plot = "desc d", _year = 1997}
     , Movie {_title = "movie e", _plot = "desc e", _year = 1997}
     , Movie {_title = "movie f", _plot = "desc f", _year = 1997} ]

searchMovie :: Text -> Maybe Movie
searchMovie t = find ((t ==) . pack . _title) movies
