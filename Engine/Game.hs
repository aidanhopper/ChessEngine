module Game where

import Data.Char
import Data.Function (on)
import Debug.Trace
import Evaluate
import Fen
import Move
import Test.HUnit
import Tests
import Utils

startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

playGame :: IO ()
playGame = helper startFen
  where
    computerTurn :: String -> IO ()
    computerTurn fenString = do
      let possiblePositions =
            concat
              $ concatMap
                ( \(moving, targets) ->
                    map (\target -> doMove moving target fenString) targets
                )
              $ filter (\(_, targets) -> targets /= [])
              $ map (\x -> (x, generateSafeMoves x fenString)) allPositions

      let evaluatedPositions =
            map (\string -> (string, evaluate string "b")) possiblePositions

      let bestPosition = fst $ head evaluatedPositions

      helper bestPosition 

    playerTurn :: String -> IO ()
    playerTurn fenString = do
      putStrLn "What piece would you like to move?"
      pieceToMove <- getLine

      let possibleMoves = generateSafeMoves (Algebraic pieceToMove) fenString

      putStrLn "Where would you like to move it?"
      whereTo <- getLine

      if Algebraic whereTo `elem` possibleMoves
        then do
          helper $
            head $
              doMove (Algebraic pieceToMove) (Algebraic whereTo) fenString
        else do
          putStrLn "INVALID MOVE... RETRY"
          helper fenString

    helper :: String -> IO ()
    helper fenString = do
      printFenString fenString

      let fen = parseFen fenString
      let side = sideToMove fen

      if side == "w"
        then playerTurn fenString
        else computerTurn fenString
