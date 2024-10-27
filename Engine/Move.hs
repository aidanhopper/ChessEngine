module Move where

import Data.Bits
import Debug.Trace
import Fen
import Utils

rankOneMask = 0xff

rankTwoMask = 0xff00

rankThreeMask = 0xff0000

rankFourMask = 0xff000000

rankFiveMask = 0xff00000000

rankSixMask = 0xff0000000000

rankSevenMask = 0xff000000000000

rankEightMask = 0xff00000000000000

fileAMask = 0x101010101010101

fileBMask = 0x202020202020202

fileCMask = 0x404040404040404

fileDMask = 0x808080808080808

fileEMask = 0x1010101010101010

fileFMask = 0x2020202020202020

fileGMask = 0x4040404040404040

fileHMask = 0x8080808080808080

-- Moves will now be generated all at once using bitwise operators.
-- This should be a lot faster.

constructMoves :: (Int -> Int) -> Bitboard -> [Move]
constructMoves toStartingIndex bb
  | bb == 0 = []
  | otherwise =
      Move
        { startingSquare = convertIndex $ toStartingIndex index,
          targetSquare = convertIndex index
        }
        : constructMoves toStartingIndex (bb `clearBit` index)
  where
    index = countTrailingZeros bb

generatePawnPush :: Board -> [Move]
generatePawnPush board =
  constructMoves (\x -> x - forward) singlePushes
    ++ constructMoves (\x -> x - forward * 2) doublePushes
    ++ constructMoves (\x -> x - east) eastAttacks
    ++ constructMoves (\x -> x - west) westAttacks
  where
    activeSide = sideToMove $ fen board
    emptySquares = complement $ allPieces board

    activePawns
      | activeSide == "w" = whitePawns board
      | otherwise = blackPawns board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    forward
      | activeSide == "w" = 8
      | otherwise = -8

    doublePushMask
      | activeSide == "w" = rankThreeMask
      | otherwise = rankSixMask

    east
      | activeSide == "w" = 7
      | otherwise = -7

    west
      | activeSide == "w" = 9
      | otherwise = -9

    singlePushes :: Bitboard
    singlePushes = emptySquares .&. shift activePawns forward

    doublePushes :: Bitboard
    doublePushes =
      emptySquares .&. shift (singlePushes .&. doublePushMask) forward

    eastAttacks :: Bitboard
    eastAttacks = 
      inactivePieces .&. shift (activePawns .&. complement fileHMask) east

    westAttacks :: Bitboard
    westAttacks =
      inactivePieces .&. shift (activePawns .&. complement fileAMask) west

generatePseudoLegalMoves :: board -> [Move]
generatePseudoLegalMoves board = []
