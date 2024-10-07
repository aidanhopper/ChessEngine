module Main where

import Data.Char

split delim [] = [""]
split delim (c : cs)
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split delim cs

-- take a fen string
-- and output all the possible moves for the side that is

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

-- 2D list to represent the board
-- ppppppp
-- 0000000
-- PPPP0PP

createBoard :: String -> [String]
createBoard str = reverse $ f str [""]
  where
    f :: String -> [String] -> [String]
    f [] acc = acc
    f (x : xs) acc
      | x /= '/' = f xs $ (head acc ++ g x) : tail acc
      | otherwise = f xs $ "" : acc
      where
        g :: Char -> String
        g '1' = " "
        g x
          | isDigit x =
              let num = read [x] :: Int
               in " " ++ (g . head . show $ (num - 1))
          | otherwise = [x]

main :: IO ()
main = do
  let fenString = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
  let parsedFen = parseFen fenString
  print . createBoard . piecePlacement $ parsedFen
  return ()
