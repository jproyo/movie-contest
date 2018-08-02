module MovieContestSpec (spec) where

import           Adapter
import           Config
import           Control.Monad
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.State  (execStateT)
import           Data.List
import           Data.Text            (Text, pack)
import           Model
import           MovieContest
import           Test.Hspec

spec :: Spec
spec = describe "tryGuess using mocking for omdbapi" $ do
    it "user should win because movie not found" $ do
        conf <- getConfig
        state <- execStateT
            (runReaderT (runContest $ tryGuess "some movie not found") conf)
            initialNotFound
        userWin state `shouldBe` True
    it "movie should be stored on state because movie found" $ do
        conf <- getConfig
        state <- execStateT
            (runReaderT (runContest $ tryGuess "movie e") conf)
            initialFound
        case state of
            PlayState _ xs Machine -> xs `shouldSatisfy` ((1==) . length)
            _                      -> expectationFailure "Should have 1 movie and Machine wins"
    it "machine should win because 3 attemps found" $ do
        conf <- getConfig
        state <- execStateT
            (runReaderT (runContest $ sequence [tryGuess "movie e" , tryGuess "movie a" , tryGuess "movie c"]) conf)
            initialFound
        finish state `shouldBe` True
    it "user should win because 2 attemps ok and after that failed" $ do
        conf <- getConfig
        state <- execStateT
            (runReaderT (runContest $ sequence [tryGuess "movie e" , tryGuess "movie a" , tryGuess "movie z"]) conf)
            initialFound
        userWin state `shouldBe` True
        finish state `shouldBe` True
        case state of
            PlayState _ xs _ -> xs `shouldSatisfy` ((2==) . length)
            _                -> expectationFailure "Should have 2 movie and Machine wins"

initialNotFound = initialStateWithHandler handlerMockNotFound

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
