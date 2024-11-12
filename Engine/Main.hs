module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Game
import Move
import Numeric (showIntAtBase)
import Utils
import Endpoints

f = "1nbqkbnr/2p2p2/6q1/3B4/8/3Q4/8/3K4 w KQkq - 0 1"

printBitboard :: Bitboard -> IO ()
printBitboard bb = do
  putStrLn $ showPrettyBitboard bb ++ "\n"

printBitboards :: [Bitboard] -> IO ()
printBitboards [] = return ()
printBitboards (bb : bbs) = do
  printBitboard bb
  printBitboards bbs

main :: IO ()
main = do
  let board = (\(Right x) -> x) $ parseBoard startFen
  print $ generatePseudoLegalMoves board
  return ()
