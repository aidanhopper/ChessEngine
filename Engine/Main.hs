module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Move
import Numeric (showIntAtBase)
import Utils

f = "8/8/7n/1n1p4/4P3/2P5/8/8 b KQkq - 0 1"

main :: IO ()
main = do
  let board = (\(Right x) -> x) $ parseBoard f
  -- putStrLn $ showPrettyBitboard $ blackPawns board
  -- putStrLn ""
  let moves = generatePseudoLegalMoves board
  print $ map (\(Move startingSquare targetSquare flags) -> (startingSquare, targetSquare)) moves
  --print $ map (\(Move startingSquare targetSquare flags) -> (startingSquare, targetSquare, flags)) moves
  return ()
