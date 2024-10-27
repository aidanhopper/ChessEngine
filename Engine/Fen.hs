module Fen where

startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

type FenString = String

data Fen = Fen
  { piecePlacement :: String,
    sideToMove :: String,
    castlingAbility :: String,
    enPassantTargetSquare :: String,
    halfmoveClock :: Int,
    fullmoveClock :: Int
  }
  deriving (Show)

emptyFen :: Fen
emptyFen =
  Fen
    { piecePlacement = "",
      sideToMove = "",
      castlingAbility = "",
      enPassantTargetSquare = "",
      halfmoveClock = 0,
      fullmoveClock = 0 
    }

parseFen :: String -> Fen
parseFen str =
  let parsedFen = split ' ' str
   in Fen
        { piecePlacement = head parsedFen,
          sideToMove = parsedFen !! 1,
          castlingAbility = parsedFen !! 2,
          enPassantTargetSquare = parsedFen !! 3,
          halfmoveClock = read (parsedFen !! 4) :: Int,
          fullmoveClock = read (parsedFen !! 5) :: Int
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
  piecePlacement fen
    ++ " "
    ++ sideToMove fen
    ++ " "
    ++ castlingAbility fen
    ++ " "
    ++ enPassantTargetSquare fen
    ++ " "
    ++ show (halfmoveClock fen)
    ++ " "
    ++ show (fullmoveClock fen)

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
