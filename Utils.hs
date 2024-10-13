{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isJust" #-}
module Utils where

import Data.Char
import Debug.Trace
import Fen

data Position = Algebraic String | Indexes (Int, Int) | None
  deriving (Eq, Show)

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

printBoard :: [String] -> IO ()
printBoard board = do
  putStrLn $ "8 |" ++ addSpaces (head board)
  putStrLn $ "7 |" ++ addSpaces (board !! 1)
  putStrLn $ "6 |" ++ addSpaces (board !! 2)
  putStrLn $ "5 |" ++ addSpaces (board !! 3)
  putStrLn $ "4 |" ++ addSpaces (board !! 4)
  putStrLn $ "3 |" ++ addSpaces (board !! 5)
  putStrLn $ "2 |" ++ addSpaces (board !! 6)
  putStrLn $ "1 |" ++ addSpaces (board !! 7)
  putStrLn "   ---------------"
  putStrLn "   a b c d e f g h"
  return ()
  where
    addSpaces [] = []
    addSpaces (c : cs) = c : ' ' : addSpaces cs

convert :: Position -> Position
convert (Algebraic target) = out
  where
    rowOut = row target
    colOut = col target

    out
      | rowOut /= Nothing && colOut /= Nothing =
          Indexes ((\(Just x) -> x) rowOut, (\(Just x) -> x) colOut)
      | otherwise = None

    col :: String -> Maybe Int
    col [x, _] = case x of
      'a' -> Just 0
      'b' -> Just 1
      'c' -> Just 2
      'd' -> Just 3
      'e' -> Just 4
      'f' -> Just 5
      'g' -> Just 6
      'h' -> Just 7
      _ -> Nothing
    col _ = Nothing

    row :: String -> Maybe Int
    row [_, x] = Just $ 8 - (read [x] :: Int)
    row _ = Nothing
convert (Indexes (row, col)) = out
  where
    extract (Just x) = x
    out
      | getCol /= Nothing && getRow /= Nothing =
          Algebraic $ extract getCol ++ extract getRow
      | otherwise = None
    getRow =
      let num = 8 - row
       in if num > 0 && num <= 8
            then Just $ show num
            else Nothing
    getCol = case col of
      0 -> Just "a"
      1 -> Just "b"
      2 -> Just "c"
      3 -> Just "d"
      4 -> Just "e"
      5 -> Just "f"
      6 -> Just "g"
      7 -> Just "h"
      _ -> Nothing

markBoard :: [(Position, Char)] -> [String] -> [String]
markBoard posList = f 0
  where
    posListIndexes :: [(Position, Char)]
    posListIndexes = map h posList

    h :: (Position, Char) -> (Position, Char)
    h (Indexes (r, c), char) = (Indexes (r, c), char)
    h (Algebraic pos, char) = (convert $ Algebraic pos, char)

    getChar :: Position -> [(Position, Char)] -> Maybe Char
    getChar _ [] = Nothing
    getChar (Indexes (i, j)) ((Indexes (r, c), char) : xs) =
      if r == i && c == j
        then Just char
        else getChar (Indexes (i, j)) xs

    g :: Int -> Int -> String -> String
    g _ _ [] = []
    g i j (col : cols) =
      if char /= Nothing
        then extract char : g i (j + 1) cols
        else col : g i (j + 1) cols
      where
        char = Indexes (i, j) `getChar` posListIndexes
        extract (Just x) = x

    f :: Int -> [String] -> [String]
    f _ [] = []
    f i (row : rows) = g i 0 row : f (i + 1) rows

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
  let board = createBoard $ piecePlacement fen
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
