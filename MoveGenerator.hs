module MoveGenerator where

import Data.Char
import Fen
import Utils

generateMove :: String -> Position String -> [String]
generateMove fenString pos =
  case side of
    "w" ->
      if isLower piece
        then [""]
        else case piece of
          'P' -> [""]
          'N' -> [""]
          'B' -> [""]
          'R' -> [""]
          'K' -> [""]
          'Q' -> [""]
    "b" ->
      if isUpper piece
        then [""]
        else case piece of
          'p' -> [""]
          'n' -> [""]
          'b' -> [""]
          'r' -> [""]
          'k' -> [""]
          'q' -> [""]
    _ -> error "Invalid side inside fen string."
  where
    fen = parseFen fenString
    board = createBoard $ piecePlacement fen
    side = sideToMove fen
    Indexes (row, col) = convert pos
    piece = board !! row !! col

    isOccupied :: Position String -> Bool
    isOccupied target =
      let Indexes (r, c) = convert target
          t = board !! r !! c
       in t /= ' '

    isOccupiedByEnemy :: Position String -> Bool
    isOccupiedByEnemy target =
      let Indexes (r, c) = convert target
          t = board !! r !! c
       in case side of
            "w" -> t /= ' ' && isLower t
            "b" -> t /= ' ' && isUpper t
            _ -> error "Invalid side."

    generatePawnMove :: [String]
    generatePawnMove = [""]
      where
        op | side == "w" = (-) | otherwise = (+)
