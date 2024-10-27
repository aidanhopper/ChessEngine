module Main where

import Move
import Utils
import Fen
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Bits

f = "8/8/8/8/p2p4/1PPP4/8/8 w KQkq - 0 1"

main :: IO ()
main = do
  let board = (\(Right x) -> x) $ parseBoard f
  putStrLn $ showPrettyBitboard $ blackPawns board
  putStrLn ""
  print $ generatePawnPush board
  return ()
