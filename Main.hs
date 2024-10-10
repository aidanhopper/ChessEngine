module Main where

import Data.Char
import Fen
import Move
import Utils

main :: IO ()
main = do
  -- let fenString = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  let fenString = "rnbqkbnr/pp1ppppp/8/1Pp5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  -- let fenString = "8/8/8/8/4R1k1/8/8/7n b KQkq c6 0 2"
  printBoard $ createBoard $ piecePlacement $ parseFen fenString
  printBoard $ doMove (Algebraic "f2") (Algebraic "f3") fenString
  return ()
