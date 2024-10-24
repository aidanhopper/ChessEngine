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

main :: IO ()
main = do
  playGame
