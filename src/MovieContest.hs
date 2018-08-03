{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

module MovieContest (
      showResult
    , executeContest
    , executeContestWithInitial
    , initialState
    , initialStateWithHandler
    , tryGuess
    , finish
    , MovieContestApp(..)
    , PlayState(..)
    , Winner(..)
) where

import           Adapter
import           Config
import           Control.Lens
import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (MonadState, StateT, execStateT, get,
                                         modify)
import           Data.Foldable          (foldMap)
import           Data.List              (length, sortOn)
import           Data.Maybe             (catMaybes)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Text.IO           (getLine)
import           Model
import qualified OmdbAdapter            as Omdb
import           Prelude                hiding (getLine)

type GuessedMovies = [Maybe Movie]

data Winner = User | Machine
    deriving (Show, Eq)

data PlayState = PlayState {
      _adapter       :: AdapterHandler
    , _guessedMovies :: GuessedMovies
    , _winner        :: Winner }

makeLenses ''PlayState

newtype MovieContestApp a = MovieContestApp {
      runContest :: ReaderT Config (StateT PlayState IO) a
    } deriving (MonadIO, MonadReader Config, MonadState PlayState)

deriving instance Functor MovieContestApp
deriving instance Monad MovieContestApp
deriving instance Applicative MovieContestApp

initialState :: PlayState
initialState = initialStateWithHandler Omdb.newHandle

initialStateWithHandler :: AdapterHandler -> PlayState
initialStateWithHandler handler = PlayState
    { _adapter = handler, _guessedMovies = [], _winner = Machine }

showMovie :: Movie -> String
showMovie m = m^.title <> " from " <> show (m^.year) <> "\n"

sortedMoviesResult :: PlayState -> [Movie]
sortedMoviesResult = sortOn (^. year) . catMaybes . (^. guessedMovies)

showMovies :: PlayState -> String
showMovies = showMovies_ . sortedMoviesResult
    where showMovies_ [] = "Anything :( :("
          showMovies_ xs = foldMap showMovie xs

showResult :: PlayState -> String
showResult p = showWinner p <> "\nThe movies I knew about: \n" <> showMovies p

showWinner :: PlayState -> String
showWinner (PlayState _ _ Machine) = "\nI win!\n"
showWinner (PlayState _ _ User)    = "I'm beaten. I don't know that movie.\n"

showMovieFound :: Movie -> String
showMovieFound m = "I know it: " <> m^.title <> "\n" <> m^.plot

finish :: PlayState -> Bool
finish (PlayState _ _ User)          = True
finish (PlayState _ guessed Machine) = (length . catMaybes) guessed == 3

updateMovie :: Maybe Movie -> MovieContestApp ()
updateMovie m = modify (over guessedMovies ((:) m))

updateWinner :: Winner -> MovieContestApp ()
updateWinner w = modify (set winner w)

beaten :: MovieContestApp ()
beaten = updateWinner User

updateGuessed :: GuessedMovies -> MovieContestApp ()
updateGuessed (Just m : _)  = liftIO $ putStrLn (showMovieFound m)
updateGuessed (Nothing : _) = beaten
updateGuessed _             = return ()

tryGuess :: Text -> MovieContestApp ()
tryGuess gs = do
    state <- get
    conf <- ask
    result <- liftIO $ runSearch (state^.adapter) conf gs
    updateMovie result


play :: MovieContestApp ()
play = do
    liftIO $ putStrLn "\nName a movie"
    liftIO getLine >>= tryGuess
    get >>= \s -> updateGuessed $ s^.guessedMovies
    state <- get
    unless (finish state) play

executeContest :: IO PlayState
executeContest =  executeContestWithInitial initialState

executeContestWithInitial :: PlayState -> IO PlayState
executeContestWithInitial initial = do
    putStrLn "I bet you can't tell me a movie I don't know in 3 tries!"
    conf <- getConfig
    execStateT (runReaderT (runContest play) conf) initial
