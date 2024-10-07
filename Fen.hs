module Fen where

-- <FEN> ::=  <Piece Placement>
--        ' ' <Side to move>
--        ' ' <Castling ability>
--        ' ' <En passant target square>
--        ' ' <Halfmove clock>
--        ' ' <Fullmove counter>
data Fen = Fen
  { piecePlacement :: String,
    sideToMove :: String,
    castlingAbility :: String,
    enPassantTargetSquare :: String,
    halfmoveClock :: Int,
    fullmoveClock :: Int
  }
  deriving (Show)

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
