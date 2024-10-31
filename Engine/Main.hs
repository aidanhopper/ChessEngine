module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Move
import Numeric (showIntAtBase)
import Utils

f = "rnbqkbnr/pppppppp/8/8/8/8/8/R3K1PR w KQkq - 0 1"

main :: IO ()
main = do
  let board = (\(Right x) -> x) $ parseBoard f
  -- putStrLn $ showPrettyBitboard $ blackPawns board
  -- putStrLn ""
  printFenString f
  let moves = generatePseudoLegalMoves board
  print $ map (\(Move startingSquare targetSquare flags) -> (startingSquare, targetSquare, flags)) moves
  --print $ map (\(Move startingSquare targetSquare flags) -> (startingSquare, targetSquare, flags)) moves
  return ()
