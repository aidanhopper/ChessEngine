module Main where

import Data.Char
import Debug.Trace
import Evaluate
import Fen
import Move
import Test.HUnit
import Tests
import Utils
import Game
import Endpoints

main :: IO ()
main = do
  let pos = search startFen 5
  --print pos
  print $ length pos
