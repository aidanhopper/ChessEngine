module Move where

import Control.Monad
import Data.Array
import Data.Bits
import Data.Word (Word64)
import Debug.Trace
import Fen
import Utils

orthogonalBlockerMasks = listArray (0, 64) $ map orthogonalBlockerMask [0 .. 63]

diagonalBlockerMasks = listArray (0, 64) $ map diagonalBlockerMask [0 .. 63]

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

    outerSquareMask = fileAMask .|. fileHMask .|. rankOneMask .|. rankEightMask

    rayNorthWest = ray index 9 outerSquareMask 0
    raySouthEast = ray index (-9) outerSquareMask 0
    rayNorthEast = ray index 7 outerSquareMask 0
    raySouthWest = ray index (-7) outerSquareMask 0

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
getOrthogonalMovesBitboard index blockers =
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
        targetBit = bit (i + step) .&. blockers

    -- draw a ray from the starting index until it hits the first 1
    -- include the 1 it hits in the output bitboard
    rayWest = ray index west fileAMask 0
    rayEast = ray index east fileHMask 0
    rayNorth = ray index north 0 0
    raySouth = ray index south 0 0

getDiagonalMovesBitboard :: Int -> Bitboard -> Bitboard
getDiagonalMovesBitboard index blockers =
  rayNorthWest .|. rayNorthEast .|. raySouthWest .|. raySouthEast
  where
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
        targetBit = bit (i + step) .&. blockers

    outerSquareMask = fileAMask .|. fileHMask .|. rankOneMask .|. rankEightMask

    rayNorthWest = ray index 9 outerSquareMask 0
    rayNorthEast = ray index 7 outerSquareMask 0
    raySouthWest = ray index (-7) outerSquareMask 0
    raySouthEast = ray index (-9) outerSquareMask 0

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
           in Move
                { startingIndex = toStartingIndex index,
                  targetIndex = index,
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

        processPiece :: Bitboard -> [Move]
        processPiece bb
          | bb == 0 = []
          | otherwise =
              let targetIndex = countTrailingZeros bb
               in Move
                    { startingIndex = index,
                      targetIndex = targetIndex,
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
              { startingIndex =
                  case activeSide of
                    "w" -> 3
                    "b" -> 59,
                targetIndex =
                  case activeSide of
                    "w" -> 1
                    "b" -> 57,
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
          activeSide == "w"
            && countTrailingZeros activeKing == 3
            && 'K'
              `elem` castleRights board
            && squaresAreEmpty
            || activeSide == "b"
              && countTrailingZeros activeKing == 59
              && 'k'
                `elem` castleRights board
              && squaresAreEmpty
        rankMask | activeSide == "w" = rankOneMask | otherwise = rankEightMask
        fileMask = fileFMask .|. fileGMask
        squaresAreEmpty =
          popCount (rankMask .&. fileMask .&. emptySquares) == 2

    constructQueenSideCastleMove :: [Move]
    constructQueenSideCastleMove
      | not isQueenSideCastleAvailable = []
      | otherwise =
          [ Move
              { startingIndex =
                  case activeSide of
                    "w" -> 3
                    "b" -> 59,
                targetIndex =
                  case activeSide of
                    "w" -> 5
                    "b" -> 61,
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
           in Move
                { startingIndex = index,
                  targetIndex = targetIndex,
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
generateRookMoves board
  | activeRooks == 0 = []
  | otherwise = constructRookMoves moves
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

    blockers = allPieces board .&. orthogonalBlockerMasks ! index
    moves = getOrthogonalMovesBitboard index blockers .&. complement activePieces

    constructRookMoves bb
      | bb == 0 = []
      | otherwise =
          let targetIndex = countTrailingZeros bb
           in Move
                { startingIndex = index,
                  targetIndex = targetIndex,
                  flags =
                    emptyFlags
                      { isCapture =
                          popCount (bit targetIndex .&. inactivePieces) == 1,
                        castleRightsToRemove =
                          case activeSide of
                            "w" -> "KQ"
                            "b" -> "kq"
                      }
                }
                : constructRookMoves (bb .&. complement (bit targetIndex))

generateBishopMoves :: Board -> [Move]
generateBishopMoves board
  | activeBishops == 0 = []
  | otherwise = constructBishopMoves moves
  where
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    index = countTrailingZeros activeBishops

    activeBishops
      | activeSide == "w" = whiteBishops board
      | otherwise = blackBishops board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    blockers = allPieces board .&. diagonalBlockerMasks ! index
    moves = getDiagonalMovesBitboard index blockers .&. complement activePieces

    constructBishopMoves bb
      | bb == 0 = []
      | otherwise =
          let targetIndex = countTrailingZeros bb
           in Move
                { startingIndex = index,
                  targetIndex = targetIndex,
                  flags =
                    emptyFlags
                      { isCapture =
                          popCount (bit targetIndex .&. inactivePieces) == 1
                      }
                }
                : constructBishopMoves (bb .&. complement (bit targetIndex))

generateQueenMoves :: Board -> [Move]
generateQueenMoves board
  | activeQueens == 0 = []
  | otherwise = constructQueenMoves moves
  where
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    index = countTrailingZeros activeQueens

    activeQueens
      | activeSide == "w" = whiteQueens board
      | otherwise = blackQueens board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    orthogonalBlockers = allPieces board .&. orthogonalBlockerMasks ! index
    diagonalBlockers = allPieces board .&. diagonalBlockerMasks ! index

    orthogonalMoves = getOrthogonalMovesBitboard index orthogonalBlockers
    diagonalMoves = getDiagonalMovesBitboard index diagonalBlockers

    moves = (orthogonalMoves .|. diagonalMoves) .&. complement activePieces

    constructQueenMoves bb
      | bb == 0 = []
      | otherwise =
          let targetIndex = countTrailingZeros bb
           in Move
                { startingIndex = index,
                  targetIndex = targetIndex,
                  flags =
                    emptyFlags
                      { isCapture =
                          popCount (bit targetIndex .&. inactivePieces) == 1
                      }
                }
                : constructQueenMoves (bb .&. complement (bit targetIndex))

generatePseudoLegalMoves :: Board -> [Move]
generatePseudoLegalMoves board =
  generatePawnMoves board
    ++ generateKnightMoves board
    ++ generateKingMoves board
    ++ generateRookMoves board
    ++ generateBishopMoves board
    ++ generateQueenMoves board

makeMove :: Board -> Move -> Board
makeMove board move = emptyBoard
  where
    hasIndex bb index = popCount (bb .&. bit index) == 1

    startingIdx = startingIndex move
    targetIdx = targetIndex move

    hasStartingIndex bb = hasIndex bb startingIdx
    hasTargetIndex bb = hasIndex bb targetIdx

    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    movingBBIsWhiteKing = hasStartingIndex (whiteKing board)
    movingBBIsWhiteQueens = hasStartingIndex (whiteQueens board)
    movingBBIsWhiteKnights = hasStartingIndex (whiteKnights board)
    movingBBIsWhiteBishops = hasStartingIndex (whiteBishops board)
    movingBBIsWhiteRooks = hasStartingIndex (whiteRooks board)
    movingBBIsWhitePawns = hasStartingIndex (whitePawns board)

    movingBBIsBlackKing = hasStartingIndex (blackKing board)
    movingBBIsBlackQueens = hasStartingIndex (blackQueens board)
    movingBBIsBlackKnights = hasStartingIndex (blackKnights board)
    movingBBIsBlackBishops = hasStartingIndex (blackBishops board)
    movingBBIsBlackRooks = hasStartingIndex (blackRooks board)
    movingBBIsBlackPawns = hasStartingIndex (blackPawns board)

    captureBBIsWhiteKing = hasStartingIndex (whiteKing board)
    captureBBIsWhiteQueens = hasStartingIndex (whiteQueens board)
    captureBBIsWhiteKnights = hasStartingIndex (whiteKnights board)
    captureBBIsWhiteBishops = hasStartingIndex (whiteBishops board)
    captureBBIsWhiteRooks = hasStartingIndex (whiteRooks board)
    captureBBIsWhitePawns = hasStartingIndex (whitePawns board)

    captureBBIsBlackKing = hasStartingIndex (blackKing board)
    captureBBIsBlackQueens = hasStartingIndex (blackQueens board)
    captureBBIsBlackKnights = hasStartingIndex (blackKnights board)
    captureBBIsBlackBishops = hasStartingIndex (blackBishops board)
    captureBBIsBlackRooks = hasStartingIndex (blackRooks board)
    captureBBIsBlackPawns = hasStartingIndex (blackPawns board)

    movingBB
      | movingBBIsWhiteKing = whiteKing board
      | movingBBIsWhiteQueens = whiteQueens board
      | movingBBIsWhiteKnights = whiteKnights board
      | movingBBIsWhiteBishops = whiteBishops board
      | movingBBIsWhiteRooks = whiteRooks board
      | movingBBIsWhitePawns = whitePawns board
      | movingBBIsBlackKing = blackKing board
      | movingBBIsBlackQueens = blackQueens board
      | movingBBIsBlackKnights = blackKnights board
      | movingBBIsBlackBishops = blackBishops board
      | movingBBIsBlackRooks = blackRooks board
      | movingBBIsBlackPawns = blackPawns board

    captureBB
      | captureBBIsWhiteKing = whiteKing board
      | captureBBIsWhiteQueens = whiteQueens board
      | captureBBIsWhiteKnights = whiteKnights board
      | captureBBIsWhiteBishops = whiteBishops board
      | captureBBIsWhiteRooks = whiteRooks board
      | captureBBIsWhitePawns = whitePawns board
      | captureBBIsBlackKing = blackKing board
      | captureBBIsBlackQueens = blackQueens board
      | captureBBIsBlackKnights = blackKnights board
      | captureBBIsBlackBishops = blackBishops board
      | captureBBIsBlackRooks = blackRooks board
      | captureBBIsBlackPawns = blackPawns board
      | otherwise = 0

    updatedMovingBB =
      movingBB
        .|. bit targetIdx .&. complement (bit startingIdx)

    updatedCaptureBB = captureBB .&. complement (bit targetIdx)

    updatedBoard
      | isDoublePawnPush moveFlags = board
      | isEnPassant moveFlags = board
      | isQueenSideCastle moveFlags = board
      | isKingSideCastle moveFlags = board
      | otherwise =
          board
            { whiteKing = updatedWhiteKing,
              whiteQueens = updatedWhiteQueens,
              whiteKnights = updatedWhiteKnights,
              whiteBishops = updatedWhiteBishops,
              whiteRooks = updatedWhiteRooks,
              whitePawns = updatedWhitePawns,
              blackKing = updatedBlackKing,
              blackQueens = updatedBlackQueens,
              blackKnights = updatedBlackKnights,
              blackBishops = updatedBlackBishops,
              blackRooks = updatedBlackRooks,
              blackPawns = updatedBlackPawns,
              sideToMove =
                case sideToMove board of
                  "w" -> "b"
                  "b" -> "w",
              castleRights = updatedCastleRights
            }
      where
        moveFlags = flags move

        updateBB moving capture def
          | moving = updatedMovingBB
          | capture = updatedCaptureBB
          | otherwise = def

        updatedWhiteKing = updateBB movingBBIsWhiteKing captureBBIsWhiteKing (whiteKing board)
        updatedWhiteQueens = updateBB movingBBIsWhiteQueens captureBBIsWhiteQueens (whiteQueens board)
        updatedWhiteKnights = updateBB movingBBIsWhiteKnights captureBBIsWhiteKnights (whiteKnights board)
        updatedWhiteBishops = updateBB movingBBIsWhiteBishops captureBBIsWhiteBishops (whiteBishops board)
        updatedWhiteRooks = updateBB movingBBIsWhiteRooks captureBBIsWhiteRooks (whiteRooks board)
        updatedWhitePawns = updateBB movingBBIsWhitePawns captureBBIsWhitePawns (whitePawns board)

        updatedBlackKing = updateBB movingBBIsBlackKing captureBBIsBlackKing (blackKing board)
        updatedBlackQueens = updateBB movingBBIsBlackQueens captureBBIsBlackQueens (blackQueens board)
        updatedBlackKnights = updateBB movingBBIsBlackKnights captureBBIsBlackKnights (blackKnights board)
        updatedBlackBishops = updateBB movingBBIsBlackBishops captureBBIsBlackBishops (blackBishops board)
        updatedBlackRooks = updateBB movingBBIsBlackRooks captureBBIsBlackRooks (blackRooks board)
        updatedBlackPawns = updateBB movingBBIsBlackPawns captureBBIsBlackPawns (blackPawns board)

        updatedCastleRights
          | castleRightsToRemove moveFlags == "" = castleRights board
          | otherwise = helper (castleRights board)
          where
            helper [] = []
            helper (x : xs)
              | x `notElem` castleRightsToRemove moveFlags = x : helper xs
              | otherwise = helper xs

-- moveFlags = flags move
-- { isDoublePawnPush :: Bool,
-- isCapture :: Bool,
-- castleRightsToRemove :: String,
-- isEnPassant :: Bool,
-- isQueenSideCastle :: Bool,
-- isKingSideCastle :: Bool
