module Move where

import Control.Debounce (trailingEdge)
import Data.Bits
import Data.Word (Word64)
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

generatePawnMoves :: Board -> [Move]
generatePawnMoves board =
  constructPawnMoves (\x -> x - forward) emptyFlags singlePushes
    ++ constructPawnMoves
      (\x -> x - forward * 2)
      (emptyFlags {isDoublePawnPush = True})
      doublePushes
    ++ constructPawnMoves
      (\x -> x - east)
      (emptyFlags {isCapture = True})
      eastAttacks
    ++ constructPawnMoves
      (\x -> x - west)
      (emptyFlags {isCapture = True})
      westAttacks
    ++ constructPawnMoves
      (\x -> x - west)
      (emptyFlags {isCapture = True, isEnPassant = True})
      enPassantAttackWest
    ++ constructPawnMoves
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

    constructPawnMoves :: (Int -> Int) -> Flags -> Bitboard -> [Move]
    constructPawnMoves toStartingIndex flags bb
      | bb == 0 = []
      | otherwise =
          Move
            { startingSquare = startingSqr,
              targetSquare = targetSqr,
              flags = flags
            }
            : constructPawnMoves toStartingIndex flags (bb `clearBit` index)
      where
        index = countTrailingZeros bb
        startingSqr = convertIndex $ toStartingIndex index
        targetSqr = convertIndex index

generateKnightMoves :: Board -> [Move]
generateKnightMoves board =
  constructKnightMoves activeKnights
  where
    activeSide = sideToMove $ fen board
    emptySquares = complement $ allPieces board
    knightMask = 0xa1100110a
    knightMaskIndex = 18

    activeKnights
      | activeSide == "w" = whiteKnights board
      | otherwise = blackKnights board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    constructKnightMoves :: Bitboard -> [Move]
    constructKnightMoves bb
      | bb == 0 = []
      | otherwise =
          processPiece knightMoves
            ++ constructKnightMoves (bb .&. complement singleKnight)
      where
        index = countTrailingZeros bb
        singleKnight = bit index
        shiftAmount = index - knightMaskIndex

        fileABCMask = fileAMask .|. fileBMask .|. fileCMask
        fileFGHMask = fileFMask .|. fileGMask .|. fileHMask

        knightMoves =
          shift knightMask shiftAmount
            .&. fileMask
            .&. complement activePieces
          where
            fileMask
              | popCount (singleKnight .&. fileABCMask) == 1 =
                  complement fileFGHMask
              | popCount (singleKnight .&. fileFGHMask) == 1 =
                  complement fileABCMask
              | otherwise = 1

        startingSqr = convertIndex index

        processPiece :: Bitboard -> [Move]
        processPiece bb
          | bb == 0 = []
          | otherwise =
              Move
                { startingSquare = startingSqr,
                  targetSquare = targetSqr,
                  flags =
                    emptyFlags
                      { isCapture =
                          popCount (bit targetIndex .&. inactivePieces) == 1
                      }
                }
                : processPiece (bb `clearBit` targetIndex)
          where
            targetIndex = countTrailingZeros bb
            targetSqr = convertIndex targetIndex

generateKingMoves :: Board -> [Move]
generateKingMoves board =
  trace (showPrettyBitboard regularKingMoves) $
    constructRegularKingMoves regularKingMoves
  where
    kingMoveMask =
      ( bit 8
          .|. bit 10
          .|. bit 0
          .|. bit 1
          .|. bit 2
          .|. bit 16
          .|. bit 17
          .|. bit 18
      ) ::
        Bitboard

    activeSide = sideToMove $ fen board
    emptySquares = complement $ allPieces board

    index = countTrailingZeros activeKing

    activeKing
      | activeSide == "w" = whiteKing board
      | otherwise = blackKing board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    regularKingMoves = shift kingMoveMask (-9 + index) .&. complement activePieces

    constructRegularKingMoves :: Bitboard -> [Move]
    constructRegularKingMoves bb
      | bb == 0 = []
      | otherwise =
          Move
            { startingSquare = convertIndex index,
              targetSquare = targetSqr,
              flags =
                emptyFlags
                  { castleRightsToRemove = "KQkq",
                    isCapture =
                      popCount
                        (bit targetIndex .&. inactivePieces)
                        == 1
                  }
            }
            : constructRegularKingMoves (bb `clearBit` targetIndex)
      where
        targetIndex = countTrailingZeros bb
        targetSqr = convertIndex targetIndex

generateSlidingMove :: Board -> [Move]
generateSlidingMove board = []

-- naming is done as ... first moves two squares, second moves one square

generatePseudoLegalMoves :: Board -> [Move]
generatePseudoLegalMoves board =
  generatePawnMoves board
    ++ generateKnightMoves board
    ++ generateKingMoves board
