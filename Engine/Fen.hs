module Fen where

startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

type FenString = String

data Fen = Fen
  { piecePlacementFen :: String,
    sideToMoveFen :: String,
    castlingAbilityFen :: String,
    enPassantTargetSquareFen :: String,
    halfmoveClockFen :: Int,
    fullmoveClockFen :: Int
  }
  deriving (Show)

emptyFen :: Fen
emptyFen =
  Fen
    { piecePlacementFen = "",
      sideToMoveFen = "",
      castlingAbilityFen = "",
      enPassantTargetSquareFen = "",
      halfmoveClockFen = 0,
      fullmoveClockFen = 0
    }

parseFen :: String -> Fen
parseFen str =
  let parsedFen = split ' ' str
   in Fen
        { piecePlacementFen = head parsedFen,
          sideToMoveFen = parsedFen !! 1,
          castlingAbilityFen = parsedFen !! 2,
          enPassantTargetSquareFen = parsedFen !! 3,
          halfmoveClockFen = read (parsedFen !! 4) :: Int,
          fullmoveClockFen = read (parsedFen !! 5) :: Int
        }
  where
    split delim [] = [""]
    split delim (c : cs)
      | c == delim = "" : rest
      | otherwise = (c : head rest) : tail rest
      where
        rest = split delim cs

buildFen :: Fen -> String
buildFen fen =
  piecePlacementFen fen
    ++ " "
    ++ sideToMoveFen fen
    ++ " "
    ++ castlingAbilityFen fen
    ++ " "
    ++ enPassantTargetSquareFen fen
    ++ " "
    ++ show (halfmoveClockFen fen)
    ++ " "
    ++ show (fullmoveClockFen fen)

removeCastlingRight :: String -> String -> String
removeCastlingRight chars rights
  | f chars rights /= "" = f chars rights
  | otherwise = "-"
  where
    f :: String -> String -> String
    f _ [] = []
    f chars (c : cs)
      | c `notElem` chars = c : f chars cs
      | otherwise = f chars cs
