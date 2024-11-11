module Game where

import Data.Maybe (isNothing)
import Move
import Utils

getMove :: [Move] -> String -> String -> Maybe Move
getMove moves startingSquare targetSquare
  | null filteredMoves = Nothing
  | otherwise = Just $ head filteredMoves
  where
    filteredMoves =
      filter
        ( \(Move startingIdx targetIdx _) ->
            convertIndex startingIdx == startingSquare
              && convertIndex targetIdx == targetSquare
        )
        moves

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
  let move = getMove moves startingSquare targetSquare
  print $ (\(Move s t _) -> (convertIndex s, convertIndex t)) <$> move

  let newBoard = makeMove board <$> move

  if isNothing newBoard
    then do
      putStrLn "Invalid move"
      playGame board
    else playGame $ (\(Just x) -> x) newBoard
