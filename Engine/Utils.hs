module Utils where

import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.Either (isLeft, isRight)
import Data.Word (Word64)
import Debug.Trace
import Fen
import Text.Printf

rankOneMask :: Bitboard
rankOneMask = 0xff

rankTwoMask :: Bitboard
rankTwoMask = 0xff00

rankThreeMask :: Bitboard
rankThreeMask = 0xff0000

rankFourMask :: Bitboard
rankFourMask = 0xff000000

rankFiveMask :: Bitboard
rankFiveMask = 0xff00000000

rankSixMask :: Bitboard
rankSixMask = 0xff0000000000

rankSevenMask :: Bitboard
rankSevenMask = 0xff000000000000

rankEightMask :: Bitboard
rankEightMask = 0xff00000000000000

ranks :: [Bitboard]
ranks =
  [ rankOneMask,
    rankTwoMask,
    rankThreeMask,
    rankFourMask,
    rankFiveMask,
    rankSixMask,
    rankSevenMask,
    rankEightMask
  ]

fileHMask :: Bitboard
fileHMask = 0x0101010101010101

fileGMask :: Bitboard
fileGMask = 0x202020202020202

fileFMask :: Bitboard
fileFMask = 0x404040404040404

fileEMask :: Bitboard
fileEMask = 0x808080808080808

fileDMask :: Bitboard
fileDMask = 0x1010101010101010

fileCMask :: Bitboard
fileCMask = 0x2020202020202020

fileBMask :: Bitboard
fileBMask = 0x4040404040404040

fileAMask :: Bitboard
fileAMask = 0x8080808080808080

files :: [Bitboard]
files =
  [ fileAMask,
    fileBMask,
    fileCMask,
    fileDMask,
    fileEMask,
    fileFMask,
    fileGMask,
    fileHMask
  ]

allSquares =
  [ col : row
    | row <- map show [8, 7 .. 1],
      col <-
        map
          (\x -> toEnum x :: Char)
          [fromEnum 'a' .. fromEnum 'h']
  ]

fileMask index =
  case index `mod` 8 of
    0 -> fileHMask
    1 -> fileGMask
    2 -> fileFMask
    3 -> fileEMask
    4 -> fileDMask
    5 -> fileCMask
    6 -> fileBMask
    7 -> fileAMask

rankMask index =
  case index `div` 8 of
    0 -> rankOneMask
    1 -> rankTwoMask
    2 -> rankThreeMask
    3 -> rankFourMask
    4 -> rankFiveMask
    5 -> rankSixMask
    6 -> rankSevenMask
    7 -> rankEightMask

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
    sideToMove :: String,
    castleRights :: String,
    enPassantTarget :: Int,
    halfmoveClock :: Int,
    fullmoveClock :: Int
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
        sideToMove
        castleRights
        enPassantTarget
        halfmoveClock
        fullmoveClock
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
        ++ ", sideToMove = "
        ++ sideToMove
        ++ ", castleRights = "
        ++ castleRights
        ++ ", enPassantTarget = "
        ++ show enPassantTarget
        ++ ", halfmoveClock = "
        ++ show halfmoveClock
        ++ ", fullmoveClock = "
        ++ show fullmoveClock
        ++ "}"

data Move = Move
  { startingIndex :: Int,
    targetIndex :: Int,
    flags :: Flags
  }
  deriving (Show)

data Flags = Flags
  { isDoublePawnPush :: Bool,
    isCapture :: Bool,
    castleRightsToRemove :: String,
    isEnPassant :: Bool,
    isQueenSideCastle :: Bool,
    isKingSideCastle :: Bool
  }
  deriving (Show)

emptyFlags =
  Flags
    { isDoublePawnPush = False,
      isCapture = False,
      castleRightsToRemove = "",
      isEnPassant = False,
      isQueenSideCastle = False,
      isKingSideCastle = False
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
      sideToMove = "",
      enPassantTarget = -1,
      castleRights = "",
      halfmoveClock = 0,
      fullmoveClock = 0
    }

pieceAt :: Board -> Int -> Char
pieceAt board index
  | isPopCountOne $ whiteKing board .&. bit index = 'K'
  | isPopCountOne $ whiteQueens board .&. bit index = 'Q'
  | isPopCountOne $ whiteRooks board .&. bit index = 'R'
  | isPopCountOne $ whiteBishops board .&. bit index = 'B'
  | isPopCountOne $ whiteKnights board .&. bit index = 'N'
  | isPopCountOne $ whitePawns board .&. bit index = 'P'
  | isPopCountOne $ blackKing board .&. bit index = 'k'
  | isPopCountOne $ blackQueens board .&. bit index = 'q'
  | isPopCountOne $ blackRooks board .&. bit index = 'r'
  | isPopCountOne $ blackBishops board .&. bit index = 'b'
  | isPopCountOne $ blackKnights board .&. bit index = 'n'
  | isPopCountOne $ blackPawns board .&. bit index = 'p'
  | otherwise = ' '
  where
    isPopCountOne n = (==) 1 $ popCount n

parseBoard :: FenString -> Either ErrorString Board
parseBoard str =
  buildBoard
    ( emptyBoard
        { sideToMove = sideToMoveFen fen,
          castleRights = castlingAbilityFen fen,
          enPassantTarget =
            countTrailingZeros $
              (\(Right x) -> x) $
                convert $
                  enPassantTargetSquareFen fen,
          halfmoveClock = halfmoveClockFen fen,
          fullmoveClock = fullmoveClockFen fen
        }
    )
    stringBoard
    0
  where
    fen = parseFen str
    stringBoard =
      concat $ createPiecePositionStringBoard $ piecePlacementFen fen
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
          0 -> "a"
          1 -> "b"
          2 -> "c"
          3 -> "d"
          4 -> "e"
          5 -> "f"
          6 -> "g"
          7 -> "h"
        row = show (8 - (index `div` 8))
        sqr = col ++ row

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
      7 -> "a"
      6 -> "b"
      5 -> "c"
      4 -> "d"
      3 -> "e"
      2 -> "f"
      1 -> "g"
      0 -> "h"
    row = show (index `div` 8 + 1)

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
  let board = createPiecePositionStringBoard $ piecePlacementFen fen
  putStrLn $
    "8 |"
      ++ addSpaces (head board)
      ++ "  side to move      "
      ++ sideToMoveFen fen
  putStrLn $
    "7 |"
      ++ addSpaces (board !! 1)
      ++ "  castling ability  "
      ++ castlingAbilityFen fen
  putStrLn $
    "6 |"
      ++ addSpaces (board !! 2)
      ++ "  enpassant square  "
      ++ enPassantTargetSquareFen fen
  putStrLn $
    "5 |"
      ++ addSpaces (board !! 3)
      ++ "  halfmove clock    "
      ++ show (halfmoveClockFen fen)
  putStrLn $
    "4 |"
      ++ addSpaces (board !! 4)
      ++ "  fullmove clock    "
      ++ show (fullmoveClockFen fen)
  putStrLn $ "3 |" ++ addSpaces (board !! 5)
  putStrLn $ "2 |" ++ addSpaces (board !! 6)
  putStrLn $ "1 |" ++ addSpaces (board !! 7)
  putStrLn "   ---------------"
  putStrLn "   a b c d e f g h"
  where
    addSpaces [] = []
    addSpaces (c : cs) = c : ' ' : addSpaces cs
