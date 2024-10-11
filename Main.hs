module Main where

import Data.Char
import Fen
import Move
import Utils

printFenString = printBoard . createBoard . piecePlacement . parseFen

main :: IO ()
main = do
  -- let fenString = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  let fenString = "rnbqkbnr/pp1ppppp/8/1Pp5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  -- let fenString = "8/8/8/8/4R1k1/8/8/7n b KQkq c6 0 2"
  let fen = parseFen fenString
  printFenString fenString
  printFenString $ head $ doMove (Algebraic "b5") (Algebraic "c6") fenString
  return ()
