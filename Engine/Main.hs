module Main where

import Data.Bits
import Data.Char (intToDigit)
import Fen
import Move
import Numeric (showIntAtBase)
import Utils

f = "rnbqkbnr/ppp1ppp1/8/3R4/2R5/8/8/4K3 w KQkq - 0 1"

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

  printBitboards $ concatMap (allBlockerBitboards . diagonalBlockerMask) [0..63]

  return ()
