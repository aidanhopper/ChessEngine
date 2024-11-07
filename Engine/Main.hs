module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Move
import Numeric (showIntAtBase)
import Utils

f = "1nbqkbnr/2p2p2/8/3B4/8/8/R7/3K3q w KQkq - 0 1"

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
  let board = (\(Right x) -> x) $ parseBoard f
  -- let index = 27
  --
  -- putStrLn $ "The blocker mask for index " ++ show index
  -- let blockerMask = orthogonalBlockerMask index
  -- printBitboard blockerMask
  --
  -- let allBlockers = allBlockerBitboards blockerMask
  -- putStrLn $ "One of the possible blocker masks for index " ++ show index
  -- let blocker = allBlockers !! 430
  -- printBitboard blocker
  --
  --
  -- putStrLn "The resulting movement mask"
  -- let moveMask = getOrthogonalMovesBitboard index blocker
  -- printBitboard moveMask

  print $ map (\(Move startingIndex targetIndex flags) -> (convertIndex startingIndex, convertIndex targetIndex)) $ generatePseudoLegalMoves board

  return ()
