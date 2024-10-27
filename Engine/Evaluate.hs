{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Evaluate where

import Data.Char
import Debug.Trace
import Fen
import Move
import Utils

allNextPossiblePositions fenString =
  concat
    $ concatMap
      ( \(moving, targets) ->
          map (\target -> doMove moving target fenString) targets
      )
    $ filter (\(_, targets) -> targets /= [])
    $ map (\x -> (x, generateMoves x fenString)) allPositions

search :: String -> Int -> [String]
search fenString depth = helper [fenString] depth
  where
    helper :: [String] -> Int -> [String]
    helper xs 0 = xs
    helper xs depth = helper (concatMap allNextPossiblePositions xs) (depth - 1)

generateBestMove :: String -> String
generateBestMove fenString = bestPosition
  where
    side = sideToMove $ parseFen fenString
    mul | side == "w" = (*) (-1) | otherwise = (*) 1

    evaluatedPositions :: [(String, Either Double String)]
    evaluatedPositions =
      map (\string -> (string, evaluate string side)) $ allNextPossiblePositions fenString

    bestEvalNum :: Double
    bestEvalNum =
      trace ("\n" ++ show (map snd evaluatedPositions) ++ "\n") f evaluatedPositions (mul 100000)
      where
        f :: [(String, Either Double String)] -> Double -> Double
        f [] n = n
        f ((_, Left num) : ps) n
          | num < n = f ps num
          | otherwise = f ps n
        f ((_, Right _) : ps) n = f ps n

    bestPosition =
      fst $ head $ filter (\x -> snd x == Left bestEvalNum) evaluatedPositions

evaluate :: String -> String -> Either Double String
evaluate fenString sideToEvaluate
  | sideToEvaluate /= "w" && sideToEvaluate /= "b" =
      Right "Invalid side as sideToEvaluate."
  | isWhiteMated = Left 1000
  | isBlackMated = Left (-1000)
  | isDraw = Left 0
  where
    fen = parseFen fenString
    fenSide = sideToMove fen
    fenPiecePlacement = piecePlacement fen

    board = createBoard fenPiecePlacement

    (bp, bn, bb, br, bk, bq) = countPieces "b"
    (wp, wn, wb, wr, wk, wq) = countPieces "w"

    whiteDoubledPawns = countDoubledPawns "w"
    blackDoubledPawns = countDoubledPawns "b"

    countPieces :: String -> (Double, Double, Double, Double, Double, Double)
    countPieces side =
      foldl
        ( \(p, n, b, r, k, q) x ->
            if isCase side x
              then case toLower x of
                'p' -> (p + 1, n, b, r, k, q)
                'n' -> (p, n + 1, b, r, k, q)
                'b' -> (p, n, b + 1, r, k, q)
                'r' -> (p, n, b, r + 1, k, q)
                'k' -> (p, n, b, r, k + 1, q)
                'q' -> (p, n, b, r, k, q + 1)
              else (p, n, b, r, k, q)
        )
        (0, 0, 0, 0, 0, 0)
        $ concat board
      where
        isCase side | side == "w" = isUpper | side == "b" = isLower

    countDoubledPawns :: String -> Double
    countDoubledPawns side = f 0
      where
        toCase side | side == "w" = toUpper | side == "b" = toLower

        f :: Int -> Double
        f c
          | c < 8 && c >= 0 =
              if out <= 1
                then f (c + 1)
                else out + f (c + 1) - 1
          | otherwise = 0
          where
            out = countPawnsInCol c

        countPawnsInCol :: Int -> Double
        countPawnsInCol c = h 0 c
          where
            h r c
              | r < 8 && c < 8 && r >= 0 && c >= 0 =
                  if board !! r !! c == toCase side 'p'
                    then 1 + h (r + 1) c
                    else h (r + 1) c
              | otherwise = 0

    isWhiteMated :: Bool
    isWhiteMated
      | sideToMove (parseFen fenString) /= "w" = False
      | null (allNextPossiblePositions fenString) = True
      | otherwise = False

    isBlackMated :: Bool
    isBlackMated
      | sideToMove (parseFen fenString) /= "b" = False
      | null (allNextPossiblePositions fenString) = True
      | otherwise = False

    isDraw :: Bool
    isDraw = False

    whiteMaterialScore :: Double
    whiteMaterialScore = 0

    blackMaterialScore :: Double
    blackMaterialScore = 0

    scoreMaterial :: Char -> Double
    scoreMaterial piece =
      case toLower piece of
        'p' -> 0
        'n' -> 0
        'b' -> 0
        'r' -> 0
        'k' -> 0
        'q' -> 0
