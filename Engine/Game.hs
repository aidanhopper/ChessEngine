module Game where

import Data.Either (isLeft)
import Move
import Utils

playGame :: Board -> IO ()
playGame board = do
  printBoard board

  -- Get possible moves from the board
  let moves = generatePseudoLegalMoves board
  putStr "Input a starting move: "
  startingSquare <- getLine
  putStr "  Input a target move: "
  targetSquare <- getLine

  -- Grab the move
  let move = getMove startingSquare targetSquare moves
  print $ (\(Move s t _) -> (convertIndex s, convertIndex t)) <$> move


  -- if isLeft newBoard
  --   then do
  --     putStrLn "Invalid move"
  --     playGame board
  --   else playGame $ (\(Right x) -> x) newBoard
