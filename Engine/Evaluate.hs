module Evaluate where

import Data.Char
import Debug.Trace
import Fen
import Move
import Utils

generateBestMove :: String -> String
generateBestMove fenString = bestPosition
  where
    possiblePositions =
      concat
        $ concatMap
          ( \(moving, targets) ->
              map (\target -> doMove moving target fenString) targets
          )
        $ filter (\(_, targets) -> targets /= [])
        $ map (\x -> (x, generateSafeMoves x fenString)) allPositions

    evaluatedPositions =
      map (\string -> (string, evaluate string "b")) possiblePositions

    bestPosition = fst $ head evaluatedPositions

evaluate :: String -> String -> Either Double String
evaluate fenString sideToEvaluate
  | sideToEvaluate /= "w" && sideToEvaluate /= "b" =
      Right "Invalid side as sideToEvaluate."
  | otherwise =
      Left $
        200 * (wp - bp)
          + 9 * (wq - wq)
          + 5 * (wr - br)
          + 3 * (wb - bb + wn - bn)
          + wp
          - bp
          - 0.5 * (whiteDoubledPawns - blackDoubledPawns)
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
