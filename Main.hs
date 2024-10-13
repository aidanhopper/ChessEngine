module Main where

import Data.Char
import Debug.Trace
import Fen
import Move
import Utils

main :: IO ()
main = do
  -- let fenString = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  let fenString = "rnbqkbnr/Pp1ppppp/8/1pp5/4P3/8/PPrP1PPP/R3KBNR w KQkq c6 0 2"

  let possibleMoves = concatMap (`generateSafeMoves` fenString) allPositions

  printFenString fenString
  print possibleMoves


  return ()
