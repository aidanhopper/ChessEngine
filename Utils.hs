{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use isJust" #-}
module Utils where

import Data.Char
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
convert (Algebraic target) = Indexes (row target, col target)
  where
    col :: String -> Int
    col [x, _] = case x of
      'a' -> 0
      'b' -> 1
      'c' -> 2
      'd' -> 3
      'e' -> 4
      'f' -> 5
      'g' -> 6
      'h' -> 7
      _ -> error "Invalid alphanumeric chess position column letter"
    col _ = error "Invalid alphanumeric chess position."
    row :: String -> Int
    row [_, x] = 8 - (read [x] :: Int)
    row _ = error "Invalid alphanumeric chess position."
convert (Indexes (row, col)) = Algebraic $ getCol ++ getRow
  where
    getRow =
      let num = 8 - row
       in if num >= 0 && num < 8
            then show num
            else error "Row is out of bounds."
    getCol = case col of
      0 -> "a"
      1 -> "b"
      2 -> "c"
      3 -> "d"
      4 -> "e"
      5 -> "f"
      6 -> "g"
      7 -> "h"
      _ -> error "Column is out of bounds."

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
