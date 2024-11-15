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
    rayWest = ray index west fileHMask 0 .&. complement fileHMask
    rayEast = ray index east fileAMask 0 .&. complement fileAMask
    rayNorth = ray index north 0 0
    raySouth = ray index south 0 0

getDiagonalMovesBitboard :: Int -> Bitboard -> Bitboard
getDiagonalMovesBitboard index blockers =
  rayNorthWest
    .|. rayNorthEast
    .|. raySouthWest
    .|. raySouthEast
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

    rayNorthWest = ray index 9 outerSquareMask 0 .&. complement fileHMask
    rayNorthEast = ray index 7 outerSquareMask 0 .&. complement fileAMask
    raySouthWest = ray index (-7) outerSquareMask 0 .&. complement fileHMask
    raySouthEast = ray index (-9) outerSquareMask 0 .&. complement fileAMask

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
      | otherwise = -9

    west
      | activeSide == "w" = 9
      | otherwise = -7

    getFileMask index =
      case index `mod` 8 of
        7 -> fileAMask
        6 -> fileBMask
        5 -> fileCMask
        4 -> fileDMask
        3 -> fileEMask
        2 -> fileFMask
        1 -> fileGMask
        0 -> fileHMask

    enPassantTargetRankMask
      | activeSide == "w" = rankFiveMask
      | activeSide == "b" = rankFourMask

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
      | enPassantTargetSquare == -1 = 0
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
      | enPassantTargetSquare == -1 = 0
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
              rank
                | activeSide == "w" = rankEightMask
                | otherwise = rankOneMask
           in Move
                { startingIndex = toStartingIndex index,
                  targetIndex = index,
                  flags =
                    flags
                      { isPawnPromotion = bit index .&. rank /= 0
                      }
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
          processPiece
            knightMoves
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
              | otherwise = complement 0

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
generateKingMoves board
  | activeKing == 0 = []
  | otherwise =
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
  | otherwise = allRookMoves activeRooks
  where
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    activeRooks
      | activeSide == "w" = whiteRooks board
      | otherwise = blackRooks board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    allRookMoves bb
      | bb == 0 = []
      | otherwise = f moves ++ allRookMoves (bb .&. complement (bit index))
      where
        index = countTrailingZeros bb
        blockers = allPieces board .&. orthogonalBlockerMasks ! index
        moves = getOrthogonalMovesBitboard index blockers .&. complement activePieces

        f movebb
          | movebb == 0 = []
          | otherwise =
              let targetIndex = countTrailingZeros movebb
               in Move
                    { startingIndex = index,
                      targetIndex = targetIndex,
                      flags =
                        emptyFlags
                          { isCapture =
                              popCount (bit targetIndex .&. inactivePieces) == 1,
                            castleRightsToRemove =
                              if index == 0
                                || index == 7
                                || index == 63
                                || index == 56
                                then case activeSide of
                                  "w" -> "KQ"
                                  "b" -> "kq"
                                else ""
                          }
                    }
                    : f (movebb .&. complement (bit targetIndex))

generateBishopMoves :: Board -> [Move]
generateBishopMoves board
  | activeBishops == 0 = []
  | otherwise = allBishopMoves activeBishops
  where
    activeSide = sideToMove board
    emptySquares = complement $ allPieces board

    activeBishops
      | activeSide == "w" = whiteBishops board
      | otherwise = blackBishops board

    inactivePieces
      | activeSide == "w" = blackPieces board
      | otherwise = whitePieces board

    activePieces
      | activeSide == "w" = whitePieces board
      | otherwise = blackPieces board

    allBishopMoves bb
      | bb == 0 = []
      | otherwise = f moves ++ allBishopMoves (bb .&. complement (bit index))
      where
        index = countTrailingZeros bb
        blockers = allPieces board .&. diagonalBlockerMasks ! index
        moves = getDiagonalMovesBitboard index blockers .&. complement activePieces

        f movebb
          | movebb == 0 = []
          | otherwise =
              let targetIndex = countTrailingZeros movebb
               in Move
                    { startingIndex = index,
                      targetIndex = targetIndex,
                      flags =
                        emptyFlags
                          { isCapture =
                              popCount (bit targetIndex .&. inactivePieces) == 1
                          }
                    }
                    : f (movebb .&. complement (bit targetIndex))

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

generateMoves :: Board -> [Move]
generateMoves board = filter isSafe $ generatePseudoLegalMoves board
  where
    kingIndex
      | sideToMove board == "w" = countTrailingZeros $ whiteKing board
      | sideToMove board == "b" = countTrailingZeros $ blackKing board
    isSafe m = kingIndex `notElem` targets
      where
        responseBoards = makeMove m board
        responseMoves = concatMap generatePseudoLegalMoves responseBoards
        targets = map targetIndex responseMoves

getPossibleMoves :: String -> Either String [Move]
getPossibleMoves fen = generateMoves <$> parseBoard fen

makeMove :: Move -> Board -> [Board]
makeMove move board = updatedBoards
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

    captureBBIsWhiteKing = hasTargetIndex (whiteKing board)
    captureBBIsWhiteQueens = hasTargetIndex (whiteQueens board)
    captureBBIsWhiteKnights = hasTargetIndex (whiteKnights board)
    captureBBIsWhiteBishops = hasTargetIndex (whiteBishops board)
    captureBBIsWhiteRooks = hasTargetIndex (whiteRooks board)
    captureBBIsWhitePawns = hasTargetIndex (whitePawns board)

    captureBBIsBlackKing = hasTargetIndex (blackKing board)
    captureBBIsBlackQueens = hasTargetIndex (blackQueens board)
    captureBBIsBlackKnights = hasTargetIndex (blackKnights board)
    captureBBIsBlackBishops = hasTargetIndex (blackBishops board)
    captureBBIsBlackRooks = hasTargetIndex (blackRooks board)
    captureBBIsBlackPawns = hasTargetIndex (blackPawns board)

    movingBB
      | isEnPassant (flags move) =
          case sideToMove board of
            "w" -> whitePawns board
            "b" -> blackPawns board
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
      | isEnPassant (flags move) =
          case sideToMove board of
            "w" -> blackPawns board
            "b" -> whitePawns board
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
      (movingBB .|. bit targetIdx) .&. complement (bit startingIdx)

    updatedCaptureBB = captureBB .&. complement (bit targetIdx)

    updatedBoards
      | isPawnPromotion moveFlags =
          [ promoToQueenBoard,
            promoToKnightBoard,
            promoToRookBoard,
            promoToBishopBoard
          ]
      | otherwise =
          [ board
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
                castleRights = updatedCastleRights,
                enPassantTarget =
                  if not (isDoublePawnPush moveFlags)
                    then -1
                    else case sideToMove board of
                      "w" -> targetIdx - 8
                      "b" -> targetIdx + 8
              }
          ]
      where
        moveFlags = flags move

        promoBoard =
          board
            { whitePawns =
                if activeSide == "w"
                  then whitePawns board .&. complement (bit startingIdx)
                  else whitePawns board,
              blackPawns =
                if activeSide == "b"
                  then blackPawns board .&. complement (bit startingIdx)
                  else blackPawns board,
              whiteKing = updatedWhiteKing,
              whiteQueens = updatedWhiteQueens,
              whiteKnights = updatedWhiteKnights,
              whiteBishops = updatedWhiteBishops,
              whiteRooks = updatedWhiteRooks,
              blackKing = updatedBlackKing,
              blackQueens = updatedBlackQueens,
              blackKnights = updatedBlackKnights,
              blackBishops = updatedBlackBishops,
              blackRooks = updatedBlackRooks,
              sideToMove =
                case sideToMove board of
                  "w" -> "b"
                  "b" -> "w",
              enPassantTarget = -1
            }

        promoToQueenBoard =
          promoBoard
            { whiteQueens =
                if activeSide == "w"
                  then whiteQueens board .|. bit targetIdx
                  else whiteQueens board,
              blackQueens =
                if activeSide == "b"
                  then blackQueens board .|. bit targetIdx
                  else blackQueens board
            }

        promoToRookBoard =
          promoBoard
            { whiteRooks =
                if activeSide == "w"
                  then whiteRooks board .|. bit targetIdx
                  else whiteRooks board,
              blackRooks =
                if activeSide == "b"
                  then blackRooks board .|. bit targetIdx
                  else blackRooks board
            }

        promoToBishopBoard =
          promoBoard
            { whiteBishops =
                if activeSide == "w"
                  then whiteBishops board .|. bit targetIdx
                  else whiteBishops board,
              blackBishops =
                if activeSide == "b"
                  then blackBishops board .|. bit targetIdx
                  else blackBishops board
            }

        promoToKnightBoard =
          promoBoard
            { whiteKnights =
                if activeSide == "w"
                  then whiteKnights board .|. bit targetIdx
                  else whiteKnights board,
              blackKnights =
                if activeSide == "b"
                  then blackKnights board .|. bit targetIdx
                  else blackKnights board
            }

        updateBB moving capture def
          | moving = updatedMovingBB
          | capture = updatedCaptureBB
          | otherwise = def

        updatedWhiteKing =
          updateBB
            movingBBIsWhiteKing
            captureBBIsWhiteKing
            (whiteKing board)

        updatedWhiteQueens =
          updateBB
            movingBBIsWhiteQueens
            captureBBIsWhiteQueens
            (whiteQueens board)

        updatedWhiteKnights =
          updateBB
            movingBBIsWhiteKnights
            captureBBIsWhiteKnights
            (whiteKnights board)

        updatedWhiteBishops =
          updateBB
            movingBBIsWhiteBishops
            captureBBIsWhiteBishops
            (whiteBishops board)

        updatedWhiteRooks
          | isQueenSideCastle moveFlags && activeSide == "w" =
              (whiteRooks board .&. complement (bit 7)) .|. bit 4
          | isKingSideCastle moveFlags && activeSide == "w" =
              (whiteRooks board .&. complement (bit 0)) .|. bit 2
          | otherwise =
              updateBB
                movingBBIsWhiteRooks
                captureBBIsWhiteRooks
                (whiteRooks board)

        updatedWhitePawns
          | not (isEnPassant moveFlags) =
              updateBB
                movingBBIsWhitePawns
                captureBBIsWhitePawns
                (whitePawns board)
          | movingBBIsWhitePawns = updatedMovingBB
          | otherwise =
              updatedCaptureBB
                .&. complement (bit (enPassantTarget board + 8))

        updatedBlackKing =
          updateBB
            movingBBIsBlackKing
            captureBBIsBlackKing
            (blackKing board)

        updatedBlackQueens =
          updateBB
            movingBBIsBlackQueens
            captureBBIsBlackQueens
            (blackQueens board)

        updatedBlackKnights =
          updateBB
            movingBBIsBlackKnights
            captureBBIsBlackKnights
            (blackKnights board)

        updatedBlackBishops =
          updateBB
            movingBBIsBlackBishops
            captureBBIsBlackBishops
            (blackBishops board)

        updatedBlackRooks
          | isQueenSideCastle moveFlags && activeSide == "b" =
              (blackRooks board .&. complement (bit 63)) .|. bit 60
          | isKingSideCastle moveFlags && activeSide == "b" =
              (blackRooks board .&. complement (bit 56)) .|. bit 58
          | otherwise =
              updateBB
                movingBBIsBlackRooks
                captureBBIsBlackRooks
                (blackRooks board)

        updatedBlackPawns
          | not (isEnPassant moveFlags) =
              updateBB
                movingBBIsBlackPawns
                captureBBIsBlackPawns
                (blackPawns board)
          | movingBBIsBlackPawns = updatedMovingBB
          | otherwise =
              updatedCaptureBB
                .&. complement (bit (enPassantTarget board - 8))

        updatedCastleRights
          | castleRights board == "-" = "-"
          | castleRightsToRemove moveFlags == "" = castleRights board
          | helper (castleRights board) == "" = "-"
          | otherwise = helper (castleRights board)
          where
            helper [] = []
            helper (x : xs)
              | x `notElem` castleRightsToRemove moveFlags = x : helper xs
              | otherwise = helper xs
