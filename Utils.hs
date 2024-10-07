module Utils where

import Data.Char
import Fen

data Position a = Algabraic a | Indexes a

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
  putStrLn $ head board
  putStrLn $ board !! 1
  putStrLn $ board !! 2
  putStrLn $ board !! 3
  putStrLn $ board !! 4
  putStrLn $ board !! 5
  putStrLn $ board !! 6
  putStrLn $ board !! 7
  return ()

convert :: Position String -> Position (Int, Int)
convert (Algabraic target) = Indexes (row target, col target)
  where
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
    row [_, x] = 8 - (read [x] :: Int)
    row _ = error "Invalid alphanumeric chess position."
