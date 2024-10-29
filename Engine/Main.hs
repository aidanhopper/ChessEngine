module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Move
import Numeric (showIntAtBase)
import Utils

f = "8/8/8/8/8/8/7P/8 w KQkq - 0 1"

main :: IO ()
main = do
  let board = (\(Right x) -> x) $ parseBoard f
  -- putStrLn $ showPrettyBitboard $ blackPawns board
  -- putStrLn ""
  let moves = generatePseudoLegalMoves board
  print $ map (\(Move startingSquare targetSquare flags) -> (startingSquare, targetSquare)) moves
  return ()
