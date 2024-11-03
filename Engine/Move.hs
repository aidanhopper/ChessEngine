module Move where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Word (Word64)
import Debug.Trace
import Fen
import Utils

orthogonalBlockerMasks = listArray (0, 63) $ map orthogonalBlockerMask [0 .. 63]

diagonalBlockerMasks = listArray (0, 63) $ map diagonalBlockerMask [0 .. 63]

orthogonalBlockerMask index = croppedRankMask .^. croppedFileMask
  where
    croppedRankMask =
      (rankMask index .&. complement fileHMask .&. complement fileAMask)
        .&. complement (bit index)
    croppedFileMask =
      (fileMask index .&. complement rankOneMask .&. complement rankEightMask)
        .&. complement (bit index)

diagonalBlockerMask index =
  rayNorthWest
    .|. raySouthEast
    .|. rayNorthEast
    .|. raySouthWest
  where
    ray :: Int -> Int -> Bitboard -> Bitboard -> Bitboard
    ray i step stopMask acc
      | i + step < 0 = acc
      | i + step > 63 = acc
      | popCount (stopMask .&. bit (i + step)) == 1 = acc
      | otherwise =
          ray
            (i + step)
            step
            stopMask
            (acc .|. bit (i + step))

    squareMask = fileAMask .|. fileHMask .|. rankOneMask .|. rankEightMask

    rayNorthWest = ray index 9 squareMask 0
    raySouthEast = ray index (-9) squareMask 0
    rayNorthEast = ray index 7 squareMask 0
    raySouthWest = ray index (-7) squareMask 0

allBlockerBitboards :: Bitboard -> [Bitboard]
allBlockerBitboards movementMask = map applyMoveSquareIndices patterns
  where
    moveSquareIndices = helper movementMask []
      where
        helper bb acc
          | bb == 0 = acc
          | otherwise =
              let index = countTrailingZeros bb
               in helper (bb `clearBit` index) (index : acc)

    patterns = replicateM (length moveSquareIndices) "01"

    applyMoveSquareIndices pattern = helper moveSquareIndices pattern 0
      where
        helper [] _ bb = bb
        helper _ [] bb = bb
        helper (moveSqr : moveSqrIndices) (x : xs) bb
          | x == '0' = helper moveSqrIndices xs bb
          | otherwise = helper moveSqrIndices xs (bb .|. bit moveSqr)

-- Expects the blocker bitboard to conform to the orthogonal blocker mask
getOrthogonalMovesBitboard :: Int -> Bitboard -> Bitboard
getOrthogonalMovesBitboard index blockerBitboard =
  rayEast .|. rayWest .|. rayNorth .|. raySouth
  where
    file = fileMask index
    rank = rankMask index
    movementMask = rank .^. file

    west = 1
    east = -1
    north = 8
    south = -8

    ray :: Int -> Int -> Bitboard -> Bitboard -> Bitboard
    ray i step stopMask acc
      | i + step < 0 = acc
      | i + step > 63 = acc
      | popCount (stopMask .&. bit (i + step)) == 1 = acc .|. bit (i + step)
      | popCount targetBit == 0 =
          ray
            (i + step)
            step
            stopMask
            (acc .|. bit (i + step))
      | popCount targetBit == 1 = acc .|. targetBit
      where
        targetBit = bit (i + step) .&. blockerBitboard

    -- draw a ray from the starting index until it hits the first 1
    -- include the 1 it hits in the output bitboard
    rayWest = ray index west fileAMask 0
    rayEast = ray index east fileHMask 0
    rayNorth = ray index north 0 0
    raySouth = ray index south 0 0

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
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board
    enPassantTargetSquare = enPassantTarget board

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

    enPassantTargetRankMask = getRankMask enPassantTargetSquare
    enPassantTargetFileMask = getFileMask enPassantTargetSquare

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
      | enPassantTargetSquare == "-" = 0
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
      | enPassantTargetSquare == "-" = 0
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
          let index = countTrailingZeros bb
              startingSqr = convertIndex $ toStartingIndex index
              targetSqr = convertIndex index
           in Move
                { startingSquare = startingSqr,
                  targetSquare = targetSqr,
                  flags = flags
                }
                : constructPawnMoves toStartingIndex flags (bb `clearBit` index)

generateKnightMoves :: Board -> [Move]
generateKnightMoves board =
  constructKnightMoves activeKnights
  where
    activeSide = sideToMove board
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
              let targetIndex = countTrailingZeros bb
                  targetSqr = convertIndex targetIndex
               in Move
                    { startingSquare = startingSqr,
                      targetSquare = targetSqr,
                      flags =
                        emptyFlags
                          { isCapture =
                              popCount (bit targetIndex .&. inactivePieces) == 1
                          }
                    }
                    : processPiece (bb `clearBit` targetIndex)

generateKingMoves :: Board -> [Move]
generateKingMoves board =
  constructRegularKingMoves regularKingMoves
    ++ constructQueenSideCastleMove
    ++ constructKingSideCastleMove
  where
    kingMoveMask :: Bitboard
    kingMoveMask =
      bit 8
        .|. bit 10
        .|. bit 0
        .|. bit 1
        .|. bit 2
        .|. bit 16
        .|. bit 17
        .|. bit 18

    activeSide = sideToMove board
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

    constructKingSideCastleMove :: [Move]
    constructKingSideCastleMove
      | not isKingSideCastleAvailable = []
      | otherwise =
          [ Move
              { startingSquare =
                  case activeSide of
                    "w" -> "e1"
                    "b" -> "e8",
                targetSquare =
                  case activeSide of
                    "w" -> "g1"
                    "b" -> "g8",
                flags =
                  emptyFlags
                    { isKingSideCastle = True,
                      castleRightsToRemove =
                        case activeSide of
                          "w" -> "KQ"
                          "b" -> "kq"
                    }
              }
          ]
      where
        isKingSideCastleAvailable =
          activeSide == "w" && 'K' `elem` castleRights board && squaresAreEmpty
            || activeSide == "b" && 'k' `elem` castleRights board && squaresAreEmpty
        rankMask | activeSide == "w" = rankOneMask | otherwise = rankEightMask
        fileMask = fileFMask .|. fileGMask
        squaresAreEmpty =
          popCount (rankMask .&. fileMask .&. emptySquares) == 2

    constructQueenSideCastleMove :: [Move]
    constructQueenSideCastleMove
      | not isQueenSideCastleAvailable = []
      | otherwise =
          [ Move
              { startingSquare =
                  case activeSide of
                    "w" -> "e1"
                    "b" -> "e8",
                targetSquare =
                  case activeSide of
                    "w" -> "c1"
                    "b" -> "c8",
                flags =
                  emptyFlags
                    { isQueenSideCastle = True,
                      castleRightsToRemove =
                        case activeSide of
                          "w" -> "KQ"
                          "b" -> "kq"
                    }
              }
          ]
      where
        isQueenSideCastleAvailable =
          activeSide == "w" && 'Q' `elem` castleRights board && squaresAreEmpty
            || activeSide == "b" && 'q' `elem` castleRights board && squaresAreEmpty
        rankMask | activeSide == "w" = rankOneMask | otherwise = rankEightMask
        fileMask = fileBMask .|. fileCMask .|. fileDMask
        squaresAreEmpty =
          popCount (rankMask .&. fileMask .&. emptySquares) == 3

    constructRegularKingMoves :: Bitboard -> [Move]
    constructRegularKingMoves bb
      | bb == 0 = []
      | otherwise =
          let targetIndex = countTrailingZeros bb
              targetSqr = convertIndex targetIndex
           in Move
                { startingSquare = convertIndex index,
                  targetSquare = targetSqr,
                  flags =
                    emptyFlags
                      { castleRightsToRemove =
                          case activeSide of
                            "w" -> "KQ"
                            "b" -> "kq",
                        isCapture =
                          popCount
                            (bit targetIndex .&. inactivePieces)
                            == 1
                      }
                }
                : constructRegularKingMoves (bb `clearBit` targetIndex)

generateRookMoves :: Board -> [Move]
generateRookMoves board = trace (showPrettyBitboard moves) []
  where
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    index = countTrailingZeros activeRooks

    activeRooks
      | activeSide == "w" = whiteRooks board
      | otherwise = blackKing board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    blockerMask = allPieces board .&. orthogonalBlockerMasks ! index
    moves = getOrthogonalMovesBitboard index blockerMask .&. complement activePieces

generatePseudoLegalMoves :: Board -> [Move]
generatePseudoLegalMoves board =
  -- generatePawnMoves board
  -- ++ generateKnightMoves board
  -- ++ generateKingMoves board
  generateRookMoves board
