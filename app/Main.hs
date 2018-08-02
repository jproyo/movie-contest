module Main where

import           MovieContest

main :: IO ()
main = executeContest >>= \s -> putStrLn $ showResult s
