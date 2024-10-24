module Game where

import Data.Char
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
    helper :: String -> IO ()
    helper fenString = do
      printFenString fenString

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

