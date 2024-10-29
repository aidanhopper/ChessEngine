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

fileHMask = 0x0101010101010101

fileGMask = 0x202020202020202

fileFMask = 0x404040404040404

fileEMask = 0x808080808080808

fileDMask = 0x1010101010101010

fileCMask = 0x2020202020202020

fileBMask = 0x4040404040404040

fileAMask = 0x8080808080808080

-- Moves will now be generated all at once using bitwise operators.
-- This should be a lot faster.

getFileMask :: Square -> Bitboard
getFileMask sqr =
  case head sqr of
    'a' -> fileAMask
    'b' -> fileBMask
    'c' -> fileCMask
    'd' -> fileDMask
    'e' -> fileEMask
    'f' -> fileFMask
    'g' -> fileGMask
    'h' -> fileHMask

getRankMask :: Square -> Bitboard
getRankMask activeSide
  | activeSide == "w" = rankFiveMask
  | activeSide == "b" = rankFourMask

-- Thinking of adding a empty pieces bitboard to the arguments
-- to check for captures. So moves will be generated such that
-- captures and non-capture moves are included in the bitboard.
-- Captures can then be tested using a bitwise AND with the
-- empty piecs argument.
constructMoves :: (Int -> Int) -> Flags -> Bitboard -> [Move]
constructMoves toStartingIndex flags bb
  | bb == 0 = []
  | otherwise =
      Move
        { startingSquare = startingSqr,
          targetSquare = targetSqr,
          flags = flags
        }
        : constructMoves toStartingIndex flags (bb `clearBit` index)
  where
    index = countTrailingZeros bb
    startingSqr = trace ("\n\n" ++ show index ++ "\n\n") convertIndex $ toStartingIndex index
    targetSqr = convertIndex index

generatePawnMoves :: Board -> [Move]
generatePawnMoves board =
  constructMoves (\x -> x - forward) emptyFlags singlePushes
    ++ constructMoves
      (\x -> x - forward * 2)
      (emptyFlags {isDoublePawnPush = True})
      doublePushes
    ++ constructMoves
      (\x -> x - east)
      (emptyFlags {isCapture = True})
      eastAttacks
    ++ constructMoves
      (\x -> x - west)
      (emptyFlags {isCapture = True})
      westAttacks
    ++ constructMoves
      (\x -> x - west)
      (emptyFlags {isCapture = True, isEnPassant = True})
      enPassantAttackWest
    ++ constructMoves
      (\x -> x - east)
      (emptyFlags {isCapture = True, isEnPassant = True})
      enPassantAttackEast
  where
    activeSide = sideToMove $ fen board
    emptySquares = complement $ allPieces board
    enPassantTarget = enPassantTargetSquare $ fen board

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

    enPassantTargetRankMask = getRankMask enPassantTarget
    enPassantTargetFileMask = getFileMask enPassantTarget

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

    enPassantAttackWest :: Bitboard
    enPassantAttackWest
      | enPassantTarget == "-" = 0
      | otherwise =
          shift
            ( shiftL activePawns 1
                .&. enPassantTargetRankMask
                .&. enPassantTargetFileMask
                .&. complement fileHMask
            )
            forward

    enPassantAttackEast :: Bitboard
    enPassantAttackEast
      | enPassantTarget == "-" = 0
      | otherwise =
          shift
            ( shiftR activePawns 1
                .&. enPassantTargetRankMask
                .&. enPassantTargetFileMask
                .&. complement fileAMask
            )
            forward

generateKnightMoves :: Board -> [Move]
generateKnightMoves board =
  trace
    (showPrettyBitboard $ knightMask .|. (\(Right x) -> x) (convert "f3"))
    []
  where
    activeSide = sideToMove $ fen board
    emptySquares = complement $ allPieces board
    knightMask = 0xa1100110a
    knightMaskCenterSquare = "f3"

    west = 1
    east = -1
    north = 8
    south = -8

    activeKnights
      | activeSide == "w" = whiteKnights board
      | otherwise = blackKnights board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

-- naming is done as ... first moves two squares, second moves one square

generatePseudoLegalMoves :: Board -> [Move]
generatePseudoLegalMoves board =
  generatePawnMoves board
    -- ++ generateKnightMoves board
