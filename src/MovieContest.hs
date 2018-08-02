{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module MovieContest (
      showResult
    , executeContest
) where

import           Control.Lens
import           Control.Monad       (unless)
import           Control.Monad.State (StateT, execStateT, get, lift, modify)
import           Data.List           (find)
import           Data.Maybe          (maybe)
import           Data.Monoid         ((<>))

newtype Guess = Guess { term :: String }

deriving instance Show Guess

data Movie = Movie { name :: String, desc :: String }
    deriving (Show, Eq, Ord)

newtype GuessedMovies = GuessedMovies [Movie]
    deriving (Show)

addMovie :: Movie -> GuessedMovies -> GuessedMovies
addMovie m (GuessedMovies ms) = GuessedMovies (m : ms)


data Winner = User | Machine
    deriving (Show, Eq)

data PlayState = PlayState {
      _tries         :: Int
    , _guessedMovies :: GuessedMovies
    , _winner        :: Winner }
    deriving (Show)

makeLenses ''PlayState

initialState :: PlayState
initialState = PlayState
    {_tries = 0, _guessedMovies = GuessedMovies [], _winner = Machine }

showMovies :: PlayState -> String
showMovies p = show $ p^.guessedMovies

showResult :: PlayState -> String
showResult p = showWinner p <> "The movies I knew about: " <> showMovies p

showWinner :: PlayState -> String
showWinner (PlayState _ _ Machine) = "I win!\n"
showWinner (PlayState _ _ User)    = "I'm beaten. I don't know that movie.\n"

updateMovie :: Movie -> MovieContestApp
updateMovie m = modify (over guessedMovies (addMovie m))

updateTries :: MovieContestApp
updateTries = modify (over tries (+1))

updateWinner :: Winner -> MovieContestApp
updateWinner w = modify (set winner w)

finish :: PlayState -> Bool
finish s = s^.tries == 3 || s^.winner == User

beaten :: MovieContestApp
beaten = updateWinner User

found :: Movie -> MovieContestApp
found movie =
    lift (putStrLn $ "I know it: " <> show movie) >> updateMovie movie

tryGuess :: String -> MovieContestApp
tryGuess gs = maybe beaten found (searchMovie gs)

type MovieContestApp = StateT PlayState IO ()

play :: MovieContestApp
play = do
    lift $ putStrLn "Name a movie"
    lift getLine >>= tryGuess >> updateTries
    state <- get
    unless (finish state) play


executeContest :: IO PlayState
executeContest = execStateT play initialState


--Testing
movies :: [Movie]
movies = [ Movie {name = "movie a", desc = "desc a"}
        , Movie {name = "movie b", desc = "desc b"}
        , Movie {name = "movie c", desc = "desc c"}
        , Movie {name = "movie d", desc = "desc d"}
        , Movie {name = "movie e", desc = "desc e"}
        , Movie {name = "movie f", desc = "desc f"} ]

searchMovie :: String -> Maybe Movie
searchMovie t = find ((t ==) . name) movies
