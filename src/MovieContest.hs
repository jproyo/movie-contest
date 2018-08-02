{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieContest (
      showResult
    , executeContest
) where

import           Adapter
import           Config
import           Control.Lens
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Control.Monad.State    (MonadState, StateT, execStateT, get,
                                         modify)
import           Data.Foldable          (foldMap)
import           Data.List              (length, sortOn)
import           Data.Maybe             (maybe)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Text.IO           (getLine)
import           Model
import           OmdbAdapter
import           Prelude                hiding (getLine)

newtype Guess = Guess { term :: Text }

deriving instance Show Guess

type GuessedMovies = [Movie]

data Winner = User | Machine
    deriving (Show, Eq)

data PlayState = PlayState {
      _guessedMovies :: GuessedMovies
    , _winner        :: Winner }
    deriving (Show)

makeLenses ''PlayState

initialState :: PlayState
initialState = PlayState
    {_guessedMovies = [], _winner = Machine }

showMovie :: Movie -> String
showMovie m = m^.title <> " from " <> show (m^.year) <> "\n"

showMovies :: PlayState -> String
showMovies = foldMap showMovie . sortOn (^. year) . (^. guessedMovies)

showResult :: PlayState -> String
showResult p = showWinner p <> "\nThe movies I knew about: \n" <> showMovies p

showWinner :: PlayState -> String
showWinner (PlayState _ Machine) = "\nI win!\n"
showWinner (PlayState _ User)    = "I'm beaten. I don't know that movie.\n"

showMovieFound :: Movie -> String
showMovieFound m = "I know it: " <> m^.title <> "\n" <> m^.plot

finish :: PlayState -> Bool
finish (PlayState _ User)          = True
finish (PlayState guessed Machine) = length guessed == 3

updateMovie :: Movie -> MovieContestApp ()
updateMovie m = modify (over guessedMovies ((:) m))

updateWinner :: Winner -> MovieContestApp ()
updateWinner w = modify (set winner w)

beaten :: MovieContestApp ()
beaten = updateWinner User

found :: Movie -> MovieContestApp ()
found movie = do
    liftIO $ putStrLn (showMovieFound movie)
    updateMovie movie


instance MovieSearcher MovieContestApp  where
    search = searchTitleOmdb

tryGuess :: Text -> MovieContestApp ()
tryGuess gs = do
    result <- search gs
    maybe beaten found result

newtype MovieContestApp a = MovieContestApp {
      runContest :: ReaderT Config (StateT PlayState IO) a
    } deriving (MonadIO, MonadReader Config, MonadState PlayState)

deriving instance Functor MovieContestApp
deriving instance Monad MovieContestApp
deriving instance Applicative MovieContestApp

play :: MovieContestApp ()
play = do
    liftIO $ putStrLn "\nName a movie"
    liftIO getLine >>= tryGuess
    state <- get
    unless (finish state) play


executeContest :: IO PlayState
executeContest = do
    putStrLn "I bet you can't tell me a movie I don't know in 3 tries!"
    conf <- getConfig
    execStateT (runReaderT (runContest play) conf) initialState


--Testing
-- movies :: [Movie]
-- movies = [ Movie {_title = "movie a", _plot = "desc a", _year = 1997}
--         , Movie {_title = "movie b", _plot = "desc b", _year = 1997}
--         , Movie {_title = "movie c", _plot = "desc c", _year = 1997}
--         , Movie {_title = "movie d", _plot = "desc d", _year = 1997}
--         , Movie {_title = "movie e", _plot = "desc e", _year = 1997}
--         , Movie {_title = "movie f", _plot = "desc f", _year = 1997} ]
--
-- searchMovie :: String -> Maybe Movie
-- searchMovie t = find ((t ==) . _title) movies
