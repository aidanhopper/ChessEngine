module Move where

import Data.Char
import Distribution.Simple.Program (ConfiguredProgram (programLocation))
import Fen
import GHC.Arr (thawSTArray)
import Utils

isWithinBoard :: Position -> Bool
isWithinBoard (Indexes (r, c)) = r >= 0 && c >= 0 && r < 8 && c < 8

{- Generates all possible moves given a fen string
 - and a input position.
 -
 - TODO get rid of moves that put the king in check
 - -}
generateMoves :: String -> Position -> [Position]
generateMoves fenString (Algebraic pos) =
  generateMoves fenString $ convert $ Algebraic pos
generateMoves fenString (Indexes (row, col)) =
  if piece == ' ' || pieceIsNotCorrectSide
    then []
    else case toLower piece of
      'p' -> generatePawnMove
      'n' -> generateKnightMove
      'b' -> generateBishopMove
      'r' -> generateRookMove
      'k' -> generateKingMove
      'q' -> generateQueenMove
      _ -> error "Invalid piece on the board."
  where
    fen = parseFen fenString
    board = createBoard $ piecePlacement fen
    side = sideToMove fen
    piece = board !! row !! col
    enPassant = enPassantTargetSquare fen
    castleRights = castlingAbility fen
    pieceIsNotCorrectSide =
      not
        ( isUpper piece && side == "w"
            || isLower piece && side == "b"
        )

    isOccupied :: Position -> Bool
    isOccupied (Algebraic target) =
      isOccupied $ convert (Algebraic target)
    isOccupied (Indexes (r, c)) =
      let t = board !! r !! c
       in isWithinBoard (Indexes (r, c)) && t /= ' '

    isOccupiedByEnemy :: Position -> Bool
    isOccupiedByEnemy (Algebraic target) =
      isOccupiedByEnemy $ convert (Algebraic target)
    isOccupiedByEnemy (Indexes (r, c)) =
      let t = board !! r !! c
       in isWithinBoard (Indexes (r, c))
            && case side of
              "w" -> t /= ' ' && isLower t
              "b" -> t /= ' ' && isUpper t
              _ -> error "Invalid."

    isNotOccupied :: Position -> Bool
    isNotOccupied (Algebraic target) = isNotOccupied $ convert (Algebraic target)
    isNotOccupied (Indexes (r, c)) = not $ isOccupied $ Indexes (r, c)

    isOccupiedByFriendly :: Position -> Bool
    isOccupiedByFriendly (Algebraic target) =
      isOccupiedByFriendly $ convert $ Algebraic target
    isOccupiedByFriendly (Indexes (r, c)) =
      let t = board !! r !! c
       in isWithinBoard (Indexes (r, c))
            && case side of
              "w" -> t /= ' ' && isUpper t
              "b" -> t /= ' ' && isLower t
              _ -> error "Invalid."

    isNotOccupiedByFriendly :: Position -> Bool
    isNotOccupiedByFriendly target =
      isWithinBoard target && not (isOccupiedByFriendly target)

    iterator :: Position -> Int -> Int -> [Position]
    iterator (Indexes (r, c)) i j
      | isWithinBoard (Indexes (r + i, c + j)) && isNotOccupied (Indexes (r + i, c + j)) = Indexes (r + i, c + j) : iterator (Indexes (r + i, c + j)) i j
      | isOccupiedByEnemy (Indexes (r + i, c + j)) = [Indexes (r + i, c + j)]
      | otherwise = []
      where
        isWithinBoardAndNotOccupied (Indexes (r, c)) =
          isWithinBoard (Indexes (r, c)) && isNotOccupied (Indexes (r, c))

    generatePawnMove :: [Position]
    generatePawnMove =
      checkForwards
        ++ checkLeap
        ++ checkAttackLeft
        ++ checkAttackRight
        ++ checkEnPassant
      where
        op | side == "w" = (-) | otherwise = (+)

        checkForwards :: [Position]
        checkForwards =
          [convert $ Indexes (row `op` 1, col) | targetIsNotOccupied]
          where
            targetIsNotOccupied = not $ isOccupied $ Indexes (row `op` 1, col)

        checkLeap :: [Position]
        checkLeap =
          [ convert $ Indexes (row `op` 2, col)
            | targetIsNotOccupied && isAtStartingPosition
          ]
          where
            isWhiteAtStartingPosition = side == "w" && row == 6
            isBlackAtStartingPosition = side == "b" && row == 1
            isAtStartingPosition =
              isWhiteAtStartingPosition || isBlackAtStartingPosition
            targetIsNotOccupied = not $ isOccupied $ Indexes (row `op` 2, col)

        checkAttackRight :: [Position]
        checkAttackRight =
          [convert $ Indexes (row `op` 1, col + 1) | targetIsOccupiedByEnemy]
          where
            targetIsOccupiedByEnemy =
              isOccupiedByEnemy $ Indexes (row `op` 1, col + 1)

        checkAttackLeft :: [Position]
        checkAttackLeft =
          [convert $ Indexes (row `op` 1, col - 1) | targetIsOccupiedByEnemy]
          where
            targetIsOccupiedByEnemy =
              isOccupiedByEnemy $ Indexes (row `op` 1, col - 1)

        checkEnPassant :: [Position]
        checkEnPassant
          | convert (Indexes (row `op` 1, col + 1)) == Algebraic enPassant =
              [Algebraic enPassant]
          | convert (Indexes (row `op` 1, col - 1)) == Algebraic enPassant =
              [Algebraic enPassant]
          | otherwise = []

    generateKnightMove :: [Position]
    generateKnightMove =
      checkDownLeft
        ++ checkDownRight
        ++ checkLeftDown
        ++ checkRightDown
        ++ checkUpLeft
        ++ checkUpRight
        ++ checkLeftUp
        ++ checkRightUp
      where
        checkDownLeft :: [Position]
        checkDownLeft =
          [ convert $ Indexes (row + 2, col - 1)
            | isNotOccupiedByFriendly $ Indexes (row + 2, col - 1)
          ]

        checkDownRight :: [Position]
        checkDownRight =
          [ convert $ Indexes (row + 2, col + 1)
            | isNotOccupiedByFriendly $ Indexes (row + 2, col + 1)
          ]

        checkLeftDown :: [Position]
        checkLeftDown =
          [ convert $ Indexes (row + 1, col - 2)
            | isNotOccupiedByFriendly $ Indexes (row + 1, col - 2)
          ]

        checkRightDown :: [Position]
        checkRightDown =
          [ convert $ Indexes (row + 1, col + 2)
            | isNotOccupiedByFriendly $ Indexes (row + 1, col + 2)
          ]

        checkUpLeft :: [Position]
        checkUpLeft =
          [ convert $ Indexes (row - 2, col - 1)
            | isNotOccupiedByFriendly $ Indexes (row - 2, col - 1)
          ]

        checkUpRight :: [Position]
        checkUpRight =
          [ convert $ Indexes (row - 2, col + 1)
            | isNotOccupiedByFriendly $ Indexes (row - 2, col + 1)
          ]

        checkLeftUp :: [Position]
        checkLeftUp =
          [ convert $ Indexes (row - 1, col - 2)
            | isNotOccupiedByFriendly $ Indexes (row - 1, col - 2)
          ]

        checkRightUp :: [Position]
        checkRightUp =
          [ convert $ Indexes (row - 1, col + 2)
            | isNotOccupiedByFriendly $ Indexes (row - 1, col + 2)
          ]

    generateBishopMove :: [Position]
    generateBishopMove =
      checkDownRight ++ checkDownLeft ++ checkUpRight ++ checkUpLeft
      where
        pos = Indexes (row, col)
        checkDownRight = iterator pos 1 1
        checkDownLeft = iterator pos 1 (-1)
        checkUpRight = iterator pos (-1) 1
        checkUpLeft = iterator pos (-1) (-1)

    generateRookMove :: [Position]
    generateRookMove = checkLeft ++ checkUp ++ checkDown ++ checkRight
      where
        pos = Indexes (row, col)
        checkLeft = iterator pos 0 (-1)
        checkUp = iterator pos (-1) 0
        checkDown = iterator pos 1 0
        checkRight = iterator pos 0 1

    -- TODO implement castling
    generateKingMove :: [Position]
    generateKingMove =
      checkUp
        ++ checkDown
        ++ checkLeft
        ++ checkRight
        ++ checkUpLeft
        ++ checkDownLeft
        ++ checkUpRight
        ++ checkDownRight
        ++ checkQueenSideCastle
        ++ checkKingSideCastle
      where
        checkQueenSideCastle =
          let num | side == "w" = "8" | side == "b" = "1"
              queenRight | side == "w" = 'Q' | side == "b" = 'q'
           in [ Algebraic ('b' : num)
                | queenRight `elem` castleRights
                    && isNotOccupied (Algebraic ('b' : num))
                    && isNotOccupied (Algebraic ('c' : num))
                    && isNotOccupied (Algebraic ('d' : num))
              ]
        checkKingSideCastle =
          let num | side == "w" = "8" | side == "b" = "1"
              kingRight | side == "w" = 'K' | side == "b" = 'k'
           in [ Algebraic ('g' : num)
                | kingRight `elem` castleRights
                    && isNotOccupied (Algebraic ('f' : num))
                    && isNotOccupied (Algebraic ('g' : num))
              ]
        checkUp =
          [ convert $ Indexes (row - 1, col)
            | isNotOccupiedByFriendly (Indexes (row - 1, col))
          ]
        checkDown =
          [ convert $ Indexes (row + 1, col)
            | isNotOccupiedByFriendly (Indexes (row + 1, col))
          ]
        checkLeft =
          [ convert $ Indexes (row, col - 1)
            | isNotOccupiedByFriendly (Indexes (row, col - 1))
          ]
        checkRight =
          [ convert $ Indexes (row, col + 1)
            | isNotOccupiedByFriendly (Indexes (row, col + 1))
          ]
        checkUpLeft =
          [ convert $ Indexes (row - 1, col - 1)
            | isNotOccupiedByFriendly (Indexes (row - 1, col - 1))
          ]
        checkUpRight =
          [ convert $ Indexes (row - 1, col + 1)
            | isNotOccupiedByFriendly (Indexes (row - 1, col + 1))
          ]
        checkDownLeft =
          [ convert $ Indexes (row + 1, col - 1)
            | isNotOccupiedByFriendly (Indexes (row + 1, col - 1))
          ]
        checkDownRight =
          [ convert $ Indexes (row + 1, col + 1)
            | isNotOccupiedByFriendly (Indexes (row + 1, col + 1))
          ]

    generateQueenMove :: [Position]
    generateQueenMove =
      checkDownRight
        ++ checkDownLeft
        ++ checkUpRight
        ++ checkUpLeft
        ++ checkLeft
        ++ checkUp
        ++ checkDown
        ++ checkRight
      where
        pos = Indexes (row, col)
        checkLeft = iterator pos 0 (-1)
        checkUp = iterator pos (-1) 0
        checkDown = iterator pos 1 0
        checkRight = iterator pos 0 1
        checkDownRight = iterator pos 1 1
        checkDownLeft = iterator pos 1 (-1)
        checkUpRight = iterator pos (-1) 1
        checkUpLeft = iterator pos (-1) (-1)

{- Take in a moving position and a target position
 - and return a list of possible fen strings that
 - move can produce (The reason for list is because
 - of pawn promotion which produces 4 different
 - possible fen strings).
 -
 - TODO Deal with wrong side trying to make a move.
 - TODO Maybe make a move validator?
 - TODO Make doMove return a list of fen strings instead of board
 - -}
doMove :: Position -> Position -> String -> [String]
doMove (Algebraic moving) (Algebraic target) fenString =
  doMove (convert (Algebraic moving)) (convert (Algebraic target)) fenString 
doMove
  (Indexes (movingRow, movingCol))
  (Indexes (targetRow, targetCol))
  fenString
    | not (isWithinBoard movingSquare) || not (isWithinBoard targetSquare) =
        error "Start or end is not within the bounds of the board."
    | otherwise = case toLower movingPiece of
        'p' -> doPawnMove
        'n' -> [""]
        'b' -> [""]
        'r' -> [""]
        'k' -> [""]
        'q' -> [""]
        _ -> error "Invalid piece on the board."
    where
      movingSquare = Indexes (movingRow, movingCol)
      targetSquare = Indexes (targetRow, targetCol)
      fen = parseFen fenString
      board = createBoard $ piecePlacement fen
      side = sideToMove fen
      enPassant = enPassantTargetSquare fen
      movingPiece = board !! movingRow !! movingCol
      targetPiece = board !! targetRow !! targetCol
      moving = convert movingSquare
      target = convert targetSquare

      doPawnMove :: [String]
      doPawnMove
        | isPawnPromo = doPawnPromoMove
        | isEnPassant = doEnPassantMove
        | isLeap = doLeapMove
        | otherwise = doRegularMove
        where
          isPawnPromo
            | side == "w" = targetRow == 0
            | side == "b" = targetRow == 7
            | otherwise = False

          isEnPassant =
            convert (Indexes (targetRow, targetCol)) == Algebraic enPassant

          isLeap
            | side == "w" && movingRow == 6 && targetRow == 4 = True
            | side == "b" && movingRow == 1 && targetRow == 3 = True
            | otherwise = False

          doPawnPromoMove :: [String]
          doPawnPromoMove = []

          doEnPassantMove :: [String]
          doEnPassantMove = []

          doLeapMove :: [String]
          doLeapMove = []

          doRegularMove :: [String]
          doRegularMove =
            markBoard [(moving, ' '), (target, movingPiece)] board
