module Main where

import Data.Char
import Debug.Trace
import Evaluate
import Fen
import Move
import Test.HUnit
import Tests
import Utils

main :: IO ()
main = do
  let fen1 = "rnbqkbnr/pp1ppppp/8/2p5/3P4/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  let fen2 = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"

  printFenString fen1
  printFenString fen2

  print $ evaluate fen1 "w"
  print $ evaluate fen2 "w"

  return ()
