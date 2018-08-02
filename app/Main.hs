module Main where

import           Control.Monad.State
import           MovieContest


main :: IO ()
main = do
  putStrLn "I bet you can't tell me a movie I don't know in 3 tries!"
  executeContest >>= \s -> putStrLn $ showResult s
