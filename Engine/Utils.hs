module Utils where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits
import Data.Char (digitToInt, isDigit)
import Data.Either (isLeft, isRight)
import Data.Word (Word64)
import Debug.Trace
import Fen
import GHC.Generics
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
  deriving (Show, Generic)

instance ToJSON Move

instance FromJSON Move

data Flags = Flags
  { isDoublePawnPush :: Bool,
    isCapture :: Bool,
    castleRightsToRemove :: String,
    isEnPassant :: Bool,
    isQueenSideCastle :: Bool,
    isKingSideCastle :: Bool
  }
  deriving (Show, Generic)

instance ToJSON Flags

instance FromJSON Flags

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

parseBoard :: FenString -> Either String Board
parseBoard str =
  buildBoard
    ( emptyBoard
        { sideToMove = sideToMoveFen fen,
          castleRights = castlingAbilityFen fen,
          enPassantTarget = ept,
          halfmoveClock = halfmoveClockFen fen,
          fullmoveClock = fullmoveClockFen fen
        }
    )
    stringBoard
    0
  where
    ept
      | convert (enPassantTargetSquareFen fen) /= 0 =
          countTrailingZeros $ convert $ enPassantTargetSquareFen fen
      | otherwise = -1
    fen = parseFen str
    stringBoard =
      concat $ createPiecePositionStringBoard $ piecePlacementFen fen
    buildBoard :: Board -> String -> Int -> Either String Board
    buildBoard board [] _ = Right board
    buildBoard board (c : cs) index =
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
        magicNumber = magicNumberBox

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

convert :: Square -> Bitboard
convert [col, row] =
  case col of
    'h' -> shiftL 0b1 shiftAmount
    'g' -> shiftL 0b10 shiftAmount
    'f' -> shiftL 0b100 shiftAmount
    'e' -> shiftL 0b1000 shiftAmount
    'd' -> shiftL 0b10000 shiftAmount
    'c' -> shiftL 0b100000 shiftAmount
    'b' -> shiftL 0b1000000 shiftAmount
    'a' -> shiftL 0b10000000 shiftAmount
    _ -> 0
  where
    shiftAmount = (digitToInt row - 1) * 8
convert "-" = 0
convert x = trace (show x) error "BAD"

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
  let board = map (map (\x -> if x == ' ' then '.' else x)) $ createPiecePositionStringBoard $ piecePlacementFen fen
  putStrLn "   abcdefgh"
  putStrLn "   --------"
  putStrLn $
    "8 |"
      ++ head board
      ++ "  side to move      "
      ++ sideToMoveFen fen
  putStrLn $
    "7 |"
      ++ board !! 1
      ++ "  castling ability  "
      ++ castlingAbilityFen fen
  putStrLn $
    "6 |"
      ++ board !! 2
      ++ "  enpassant square  "
      ++ enPassantTargetSquareFen fen
  putStrLn $
    "5 |"
      ++ board !! 3
      ++ "  halfmove clock    "
      ++ show (halfmoveClockFen fen)
  putStrLn $
    "4 |"
      ++ board !! 4
      ++ "  fullmove clock    "
      ++ show (fullmoveClockFen fen)
  putStrLn $ "3 |" ++ board !! 5
  putStrLn $ "2 |" ++ board !! 6
  putStrLn $ "1 |" ++ board !! 7
  putStrLn "   --------"
  putStrLn "   abcdefgh"
  where
    addSpaces [] = []
    addSpaces (c : cs) = c : ' ' : addSpaces cs

-- boardToFenString :: Board -> String
boardToFenString board =
  piecePlacementString
    ++ " "
    ++ sideToMove board
    ++ " "
    ++ castleRights board
    ++ " "
    ++ enPassantSquareString
    ++ " "
    ++ show (halfmoveClock board)
    ++ " "
    ++ show (fullmoveClock board)
  where
    -- get pieces from the board
    pieces = map (pieceAt board) [0 .. 63]

    -- split pieces into an 8x8 2d list
    splitPieces = split pieces ""
      where
        split :: String -> String -> [String]
        split [] acc = [acc]
        split (c : cs) acc
          | length acc == 8 = acc : split cs [c]
          | otherwise = split cs (c : acc)

    -- create function that takes a row and maps its spaces to numbers
    g row = f row 0
      where
        f [] acc
          | acc == 0 = ""
          | otherwise = show acc
        f (c : cs) acc
          | c == ' ' = f cs (acc + 1)
          | acc == 0 = c : f cs 0
          | otherwise = head (show acc) : c : f cs 0

    mapped = reverse $ map g splitPieces

    piecePlacementString = tail $ foldr (\x acc -> "/" ++ x ++ acc) "" mapped

    enPassantSquareString
      | enPassantTarget board == -1 = "-"
      | otherwise = convertIndex $ enPassantTarget board

printBoard = printFenString . boardToFenString

getMove :: String -> String -> [Move] -> Either String Move
getMove startingSquare targetSquare moves
  | null filteredMoves = Left "Move does not exist"
  | otherwise = Right $ head filteredMoves
  where
    filteredMoves =
      filter
        ( \(Move startingIdx targetIdx _) ->
            convertIndex startingIdx == startingSquare
              && convertIndex targetIdx == targetSquare
        )
        moves
