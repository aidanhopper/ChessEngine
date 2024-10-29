module Utils where

import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.Either (isLeft, isRight)
import Data.Word (Word64)
import Debug.Trace
import Fen
import Text.Printf

allSquares =
  [ col : row
    | row <- map show [8, 7 .. 1],
      col <-
        map
          (\x -> toEnum x :: Char)
          [fromEnum 'a' .. fromEnum 'h']
  ]

type Bitboard = Word64

type Square = String

type ErrorString = String

data Board = Board
  { whiteKing :: Bitboard,
    whiteQueens :: Bitboard,
    whiteRooks :: Bitboard,
    whiteBishops :: Bitboard,
    whiteKnights :: Bitboard,
    whitePawns :: Bitboard,
    whitePieces :: Bitboard,
    blackKing :: Bitboard,
    blackQueens :: Bitboard,
    blackRooks :: Bitboard,
    blackBishops :: Bitboard,
    blackKnights :: Bitboard,
    blackPawns :: Bitboard,
    blackPieces :: Bitboard,
    allPieces :: Bitboard,
    fen :: Fen
  }

showBitboard :: Bitboard -> String
showBitboard b = pad $ printf "%b" b
  where
    pad str =
      let padlen = 64 - length str in ['0' | x <- [1 .. padlen]] ++ str

showPrettyBitboard :: Bitboard -> String
showPrettyBitboard b = prettifyBitboardString
  where
    bstr = foldl (\acc x -> (x, length acc + 1) : acc) [] $ showBitboard b
    prettifyBitboardString =
      map (\x -> if x == '0' then '.' else x) $
        foldl
          ( \acc x ->
              if snd x `mod` 8 == 0 && snd x /= 64 then fst x : '\n' : acc else fst x : acc
          )
          []
          bstr

instance Show Board where
  show
    ( Board
        whiteKing
        whiteQueens
        whiteRooks
        whiteBishops
        whiteKnights
        whitePawns
        whitePieces
        blackKing
        blackQueens
        blackRooks
        blackBishops
        blackKnights
        blackPawns
        blackPieces
        allPieces
        fen
      ) =
      "Board {whiteKing = "
        ++ showBitboard whiteKing
        ++ ", whiteQueens = "
        ++ showBitboard whiteQueens
        ++ ", whiteRooks = "
        ++ showBitboard whiteRooks
        ++ ", whiteBishops = "
        ++ showBitboard whiteBishops
        ++ ", whiteKnights = "
        ++ showBitboard whiteKnights
        ++ ", whitePawns = "
        ++ showBitboard whitePawns
        ++ ", whitePieces = "
        ++ showBitboard whitePieces
        ++ ", blackKing = "
        ++ showBitboard blackKing
        ++ ", blackQueens = "
        ++ showBitboard blackQueens
        ++ ", blackRooks = "
        ++ showBitboard blackRooks
        ++ ", blackBishops = "
        ++ showBitboard blackBishops
        ++ ", blackKnights = "
        ++ showBitboard blackKnights
        ++ ", blackPawns = "
        ++ showBitboard blackPawns
        ++ ", blackPieces = "
        ++ showBitboard blackPieces
        ++ ", allPieces = "
        ++ showBitboard allPieces
        ++ ", fen = "
        ++ show fen

data Move = Move
  { startingSquare :: Square,
    targetSquare :: Square,
    flags :: Flags
  }
  deriving (Show)

data Flags = Flags
  { isDoublePawnPush :: Bool,
    isCapture :: Bool,
    castleRightsToRemove :: String,
    isEnPassant :: Bool
  }
  deriving (Show)

emptyFlags =
  Flags
    { isDoublePawnPush = False,
      isCapture = False,
      castleRightsToRemove = "",
      isEnPassant = False
    }

emptyBoard =
  Board
    { whiteKing = 0,
      whiteQueens = 0,
      whiteRooks = 0,
      whiteBishops = 0,
      whiteKnights = 0,
      whitePawns = 0,
      whitePieces = 0,
      blackKing = 0,
      blackQueens = 0,
      blackRooks = 0,
      blackBishops = 0,
      blackKnights = 0,
      blackPawns = 0,
      blackPieces = 0,
      allPieces = 0,
      fen = emptyFen
    }

pieceAt :: Square -> Board -> Either ErrorString Char
pieceAt sqr board
  | isLeft magicNumberBox = Left "Invalid square as input."
  | isPopCountOne $ whiteKing board .&. magicNumber = Right 'K'
  | isPopCountOne $ whiteQueens board .&. magicNumber = Right 'Q'
  | isPopCountOne $ whiteRooks board .&. magicNumber = Right 'R'
  | isPopCountOne $ whiteBishops board .&. magicNumber = Right 'B'
  | isPopCountOne $ whiteKnights board .&. magicNumber = Right 'N'
  | isPopCountOne $ whitePawns board .&. magicNumber = Right 'P'
  | isPopCountOne $ blackKing board .&. magicNumber = Right 'k'
  | isPopCountOne $ blackQueens board .&. magicNumber = Right 'q'
  | isPopCountOne $ blackRooks board .&. magicNumber = Right 'r'
  | isPopCountOne $ blackBishops board .&. magicNumber = Right 'b'
  | isPopCountOne $ blackKnights board .&. magicNumber = Right 'n'
  | isPopCountOne $ blackPawns board .&. magicNumber = Right 'p'
  | otherwise = Right '.'
  where
    magicNumberBox = convert sqr
    magicNumber = (\(Right x) -> x) magicNumberBox
    isPopCountOne n = (==) 1 $ popCount n

parseBoard :: FenString -> Either ErrorString Board
parseBoard str = buildBoard (emptyBoard {fen = fen}) stringBoard 0
  where
    fen = parseFen str
    stringBoard =
      concat $ createPiecePositionStringBoard $ piecePlacement fen
    buildBoard :: Board -> String -> Int -> Either ErrorString Board
    buildBoard board [] _ = Right board
    buildBoard board (c : cs) index
      | isLeft magicNumberBox =
          Left "Something went wrong with the magic number conversion in parseBoard."
      | otherwise =
          case c of
            'K' ->
              buildBoard
                ( whiteBoard {whiteKing = whiteKing whiteBoard .|. magicNumber}
                )
                cs
                (index + 1)
            'Q' ->
              buildBoard
                (whiteBoard {whiteQueens = whiteQueens whiteBoard .|. magicNumber})
                cs
                (index + 1)
            'R' ->
              buildBoard
                (whiteBoard {whiteRooks = whiteRooks whiteBoard .|. magicNumber})
                cs
                (index + 1)
            'B' ->
              buildBoard
                (whiteBoard {whiteBishops = whiteBishops whiteBoard .|. magicNumber})
                cs
                (index + 1)
            'N' ->
              buildBoard
                (whiteBoard {whiteKnights = whiteKnights whiteBoard .|. magicNumber})
                cs
                (index + 1)
            'P' ->
              buildBoard
                (whiteBoard {whitePawns = whitePawns whiteBoard .|. magicNumber})
                cs
                (index + 1)
            'k' ->
              buildBoard
                (blackBoard {blackKing = blackPawns blackBoard .|. magicNumber})
                cs
                (index + 1)
            'q' ->
              buildBoard
                (blackBoard {blackQueens = blackQueens blackBoard .|. magicNumber})
                cs
                (index + 1)
            'r' ->
              buildBoard
                ( blackBoard {blackRooks = blackRooks blackBoard .|. magicNumber}
                )
                cs
                (index + 1)
            'b' ->
              buildBoard
                (blackBoard {blackBishops = blackBishops blackBoard .|. magicNumber})
                cs
                (index + 1)
            'n' ->
              buildBoard
                (blackBoard {blackKnights = blackKnights blackBoard .|. magicNumber})
                cs
                (index + 1)
            'p' ->
              buildBoard
                (blackBoard {blackPawns = blackPawns blackBoard .|. magicNumber})
                cs
                (index + 1)
            _ -> buildBoard board cs (index + 1)
      where
        col = case index `mod` 8 of
          0 -> "h"
          1 -> "g"
          2 -> "f"
          3 -> "e"
          4 -> "d"
          5 -> "c"
          6 -> "b"
          7 -> "a"
        row = show (8 - (index `div` 8))
        sqr = convertIndex index

        magicNumberBox = convert sqr
        magicNumber = (\(Right x) -> x) magicNumberBox

        allPiecesBoard = board {allPieces = allPieces board .|. magicNumber}
        whiteBoard = allPiecesBoard {whitePieces = whitePieces board .|. magicNumber}
        blackBoard = allPiecesBoard {blackPieces = blackPieces board .|. magicNumber}

createPiecePositionStringBoard :: String -> [String]
createPiecePositionStringBoard str = reverse $ f str [""]
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

convert :: Square -> Either ErrorString Bitboard
convert [col, row]
  | not $ isDigit row = Left "Invalid row. Not a digit."
  | digitToInt row <= 0 || digitToInt row > 8 =
      Left ("Invalid row " ++ [col, row] ++ ". Out of bounds number.")
  | otherwise =
      case col of
        'h' -> Right $ shiftL 0b1 shiftAmount
        'g' -> Right $ shiftL 0b10 shiftAmount
        'f' -> Right $ shiftL 0b100 shiftAmount
        'e' -> Right $ shiftL 0b1000 shiftAmount
        'd' -> Right $ shiftL 0b10000 shiftAmount
        'c' -> Right $ shiftL 0b100000 shiftAmount
        'b' -> Right $ shiftL 0b1000000 shiftAmount
        'a' -> Right $ shiftL 0b10000000 shiftAmount
        _ -> Left ("Invalid letter, input col was " ++ [col] ++ ".")
  where
    shiftAmount = (digitToInt row - 1) * 8
convert _ = Left "Invalid, input must have a length of 2."

convertIndex :: Int -> Square
convertIndex index = col ++ row
  where
    col = case index `mod` 8 of
      7 -> "h"
      6 -> "g"
      5 -> "g"
      4 -> "e"
      3 -> "d"
      2 -> "c"
      1 -> "b"
      0 -> "a"
    row = show (8 - index `div` 8)

toPiecePlacement :: [String] -> String
toPiecePlacement board =
  tail $ foldl (\x acc -> x ++ "/" ++ acc) "" (g board)
  where
    g [] = []
    g (row : rows) = f row 0 : g rows
    f [] n
      | n == 0 = []
      | otherwise = [head (show n)]
    f (col : cols) n
      | col == ' ' = f cols (n + 1)
      | n == 0 = col : f cols 0
      | otherwise = head (show n) : col : f cols 0

printFenString :: String -> IO ()
printFenString fenString = do
  let fen = parseFen fenString
  let board = createPiecePositionStringBoard $ piecePlacement fen
  putStrLn $
    "8 |"
      ++ addSpaces (head board)
      ++ "  side to move      "
      ++ sideToMove fen
  putStrLn $
    "7 |"
      ++ addSpaces (board !! 1)
      ++ "  castling ability  "
      ++ castlingAbility fen
  putStrLn $
    "6 |"
      ++ addSpaces (board !! 2)
      ++ "  enpassant square  "
      ++ enPassantTargetSquare fen
  putStrLn $
    "5 |"
      ++ addSpaces (board !! 3)
      ++ "  halfmove clock    "
      ++ show (halfmoveClock fen)
  putStrLn $
    "4 |"
      ++ addSpaces (board !! 4)
      ++ "  fullmove clock    "
      ++ show (fullmoveClock fen)
  putStrLn $ "3 |" ++ addSpaces (board !! 5)
  putStrLn $ "2 |" ++ addSpaces (board !! 6)
  putStrLn $ "1 |" ++ addSpaces (board !! 7)
  putStrLn "   ---------------"
  putStrLn "   a b c d e f g h"
  where
    addSpaces [] = []
    addSpaces (c : cs) = c : ' ' : addSpaces cs
