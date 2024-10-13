module Move where

import Data.Char
import Debug.Trace
import Fen
import Utils

isWithinBoard :: Position -> Bool
isWithinBoard (Indexes (r, c)) = r >= 0 && c >= 0 && r < 8 && c < 8

allPositions :: [Position]
allPositions = [convert $ Indexes (i, j) | i <- [0 .. 7], j <- [0 .. 7]]

attackablePositions :: String -> [Position]
attackablePositions fenString = concatMap (`generateMoves` fenString) allPositions

pieceAt :: String -> Position -> Maybe Char
pieceAt fenString (Algebraic pos) = pieceAt fenString (convert (Algebraic pos))
pieceAt fenString (Indexes (i, j))
  | isWithinBoard (Indexes (i, j)) = Just piece
  | otherwise = Nothing
  where
    piece = board !! i !! j
    fen = parseFen fenString
    board = createBoard $ piecePlacement fen

piecePositions :: Char -> String -> [Position]
piecePositions char fenString =
  filter (\x -> pieceAt fenString x == Just char) allPositions

generateSafeMoves :: Position -> String -> [Position]
generateSafeMoves pos fenString =
  filterSafeMoves pos fenString $ generateMoves pos fenString

filterSafeMoves :: Position -> String -> [Position] -> [Position]
filterSafeMoves moving fenString possibleMoves = out
  where
    fen = parseFen fenString
    sideCase | sideToMove fen == "w" = toUpper | otherwise = toLower
    performMove x = (x, head $ doMove moving x fenString)
    getAttackablePositions x = (fst x, snd x, attackablePositions $ snd x)
    movesThatPutKingInCheck (x, string, attackables) =
      (x, head (piecePositions (sideCase 'k') string) `notElem` attackables)
    badMove (x, putsKingInCheck) = putsKingInCheck
    out =
      map fst $
        filter badMove $
          map
            ( movesThatPutKingInCheck
                . getAttackablePositions
                . performMove
            )
            possibleMoves

{- Generates all possible moves given a fen string
 - and a input position.
 -
 - TODO get rid of moves that put the king in check
 - -}
generateMoves :: Position -> String -> [Position]
generateMoves (Algebraic pos) fenString =
  generateMoves (convert $ Algebraic pos) fenString
generateMoves (Indexes (row, col)) fenString =
  filter (/= None) $
    generateMovesHelper (Indexes (row, col)) fenString
  where
    generateMovesHelper (Indexes (row, col)) fenString =
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

    moving = Indexes (row, col)
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
      | isWithinBoard (Indexes (r + i, c + j))
          && isNotOccupied (Indexes (r + i, c + j)) =
          convert (Indexes (r + i, c + j)) : iterator (Indexes (r + i, c + j)) i j
      | isOccupiedByEnemy (Indexes (r + i, c + j)) =
          [convert $ Indexes (r + i, c + j)]
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
          [convert pos | targetIsNotOccupied && isAtStartingPosition]
          where
            pos = Indexes (row `op` 2, col)
            isWhiteAtStartingPosition = side == "w" && row == 6
            isBlackAtStartingPosition = side == "b" && row == 1
            isAtStartingPosition =
              isWhiteAtStartingPosition || isBlackAtStartingPosition
            targetIsNotOccupied = not $ isOccupied pos

        checkAttackRight :: [Position]
        checkAttackRight =
          [convert pos | targetIsOccupiedByEnemy]
          where
            pos = Indexes (row `op` 1, col + 1)
            targetIsOccupiedByEnemy = isOccupiedByEnemy pos

        checkAttackLeft :: [Position]
        checkAttackLeft =
          [convert pos | targetIsOccupiedByEnemy]
          where
            pos = Indexes (row `op` 1, col - 1)
            targetIsOccupiedByEnemy = isOccupiedByEnemy pos

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
          let num | side == "w" = "1" | side == "b" = "8"
              queenRight | side == "w" = 'Q' | side == "b" = 'q'
           in [ Algebraic ('c' : num)
                | queenRight `elem` castleRights
                    && isNotOccupied (Algebraic ('b' : num))
                    && isNotOccupied (Algebraic ('c' : num))
                    && isNotOccupied (Algebraic ('d' : num))
              ]
        checkKingSideCastle =
          let num | side == "w" = "1" | side == "b" = "8"
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
 - -}
doMove :: Position -> Position -> String -> [String]
doMove (Algebraic moving) (Algebraic target) fenString =
  doMove
    (convert (Algebraic moving))
    (convert (Algebraic target))
    fenString
doMove
  (Indexes (movingRow, movingCol))
  (Indexes (targetRow, targetCol))
  fenString
    | not (isWithinBoard movingSquare) || not (isWithinBoard targetSquare) =
        error "Start or end is not within the bounds of the board."
    | otherwise = case toLower movingPiece of
        'p' -> doPawnMove
        'n' -> doKnightMove
        'b' -> doBishopMove
        'r' -> doRookMove
        'k' -> doKingMove
        'q' -> doQueenMove
        _ -> error "Invalid piece on the board."
    where
      movingSquare = Indexes (movingRow, movingCol)
      targetSquare = Indexes (targetRow, targetCol)
      fen = parseFen fenString
      board = createBoard $ piecePlacement fen
      castleRights = castlingAbility fen
      side = sideToMove fen
      enPassant = enPassantTargetSquare fen
      movingPiece = board !! movingRow !! movingCol
      targetPiece = board !! targetRow !! targetCol
      moving = convert movingSquare
      target = convert targetSquare
      halfmove = halfmoveClock fen
      fullmove = fullmoveClock fen
      isCapture = targetPiece /= ' '
      toSidesCase char
        | side == "w" = toUpper char
        | side == "b" = toLower char
      newFenBase =
        fen
          { sideToMove = if side == "w" then "b" else "w",
            enPassantTargetSquare = "-",
            fullmoveClock = if side == "b" then fullmove + 1 else fullmove,
            halfmoveClock = if isCapture then 0 else halfmove + 1
          }

      doPawnMove :: [String]
      doPawnMove
        | isPawnPromo = doPawnPromoMove
        | isEnPassant = doEnPassantMove
        | isLeap = doLeapMove
        | otherwise = doRegularMove
        where
          op | side == "w" = (-) | otherwise = (+)

          isPawnPromo
            | side == "w" = targetRow == 0
            | side == "b" = targetRow == 7
            | otherwise = error "Invalid side in fen string."

          isEnPassant =
            convert (Indexes (targetRow, targetCol)) == Algebraic enPassant

          isLeap
            | side == "w" && movingRow == 6 && targetRow == 4 = True
            | side == "b" && movingRow == 1 && targetRow == 3 = True
            | otherwise = False

          doPawnPromoMove :: [String]
          doPawnPromoMove =
            [ buildFen $ promoFenBase {piecePlacement = newPromoPiecePlacement 'q'},
              buildFen $ promoFenBase {piecePlacement = newPromoPiecePlacement 'n'},
              buildFen $ promoFenBase {piecePlacement = newPromoPiecePlacement 'r'},
              buildFen $ promoFenBase {piecePlacement = newPromoPiecePlacement 'b'}
            ]
            where
              promoFenBase = newFenBase {halfmoveClock = 0}
              newPromoPiecePlacement :: Char -> String
              newPromoPiecePlacement char =
                toPiecePlacement $
                  markBoard
                    [(moving, ' '), (target, toSidesCase char)]
                    board

          doEnPassantMove :: [String]
          doEnPassantMove =
            [ buildFen $
                newFenBase
                  { piecePlacement = enPassantPiecePlacement,
                    halfmoveClock = 0
                  }
            ]
            where
              enPassantPiecePlacement =
                toPiecePlacement $
                  markBoard
                    [ (moving, ' '),
                      (target, movingPiece),
                      ( convert $
                          Indexes (targetRow `op` (-1), targetCol),
                        ' '
                      )
                    ]
                    board

          doLeapMove :: [String]
          doLeapMove =
            [ buildFen $
                newFenBase
                  { piecePlacement =
                      toPiecePlacement $
                        markBoard [(moving, ' '), (target, movingPiece)] board,
                    enPassantTargetSquare =
                      (\(Algebraic x) -> x) $
                        convert (Indexes (movingRow `op` 1, movingCol)),
                    halfmoveClock = 0
                  }
            ]

          doRegularMove :: [String]
          doRegularMove =
            [ buildFen $
                newFenBase
                  { piecePlacement =
                      toPiecePlacement $
                        markBoard [(moving, ' '), (target, movingPiece)] board,
                    halfmoveClock = 0
                  }
            ]

      doKnightMove :: [String]
      doKnightMove =
        [ buildFen $
            newFenBase
              { piecePlacement =
                  toPiecePlacement $
                    markBoard [(moving, ' '), (target, movingPiece)] board
              }
        ]

      doBishopMove :: [String]
      doBishopMove =
        [ buildFen $
            newFenBase
              { piecePlacement =
                  toPiecePlacement $
                    markBoard [(moving, ' '), (target, movingPiece)] board
              }
        ]

      doRookMove :: [String]
      doRookMove =
        [ buildFen $
            newFenBase
              { piecePlacement =
                  toPiecePlacement $
                    markBoard [(moving, ' '), (target, movingPiece)] board,
                castlingAbility = newCastleRights
              }
        ]
        where
          newCastleRights
            | side == "w" = removeCastlingRight "QK" castleRights
            | side == "b" = removeCastlingRight "qk" castleRights

      doKingMove :: [String]
      doKingMove =
        [ buildFen $
            newFenBase
              { piecePlacement = toPiecePlacement newBoard,
                castlingAbility = newCastleRights
              }
        ]
        where
          isQueenSideWhiteCastleMove =
            side == "w"
              && 'Q' `elem` castleRights
              && moving == Algebraic "e1"
              && target == Algebraic "c1"
          isQueenSideBlackCastleMove =
            side == "b"
              && 'q' `elem` castleRights
              && moving == Algebraic "e8"
              && target == Algebraic "c8"
          isKingSideWhiteCastleMove =
            side == "w"
              && 'K' `elem` castleRights
              && moving == Algebraic "e1"
              && target == Algebraic "g1"
          isKingSideBlackCastleMove =
            side == "b"
              && 'k' `elem` castleRights
              && moving == Algebraic "e8"
              && target == Algebraic "g8"
          newCastleRights
            | side == "w" = removeCastlingRight "QK" castleRights
            | side == "b" = removeCastlingRight "qk" castleRights
          newBoard
            | isQueenSideWhiteCastleMove =
                markBoard
                  [ (moving, ' '),
                    (target, movingPiece),
                    (Algebraic "a1", ' '),
                    (Algebraic "d1", 'R')
                  ]
                  board
            | isQueenSideBlackCastleMove =
                markBoard
                  [ (moving, ' '),
                    (target, movingPiece),
                    (Algebraic "h1", ' '),
                    (Algebraic "f1", 'R')
                  ]
                  board
            | isKingSideWhiteCastleMove =
                markBoard
                  [ (moving, ' '),
                    (target, movingPiece),
                    (Algebraic "a8", ' '),
                    (Algebraic "d8", 'r')
                  ]
                  board
            | isKingSideBlackCastleMove =
                markBoard
                  [ (moving, ' '),
                    (target, movingPiece),
                    (Algebraic "h8", ' '),
                    (Algebraic "f8", 'r')
                  ]
                  board
            | otherwise =
                markBoard [(moving, ' '), (target, movingPiece)] board

      doQueenMove :: [String]
      doQueenMove =
        [ buildFen $
            newFenBase
              { piecePlacement =
                  toPiecePlacement $
                    markBoard [(moving, ' '), (target, movingPiece)] board
              }
        ]
