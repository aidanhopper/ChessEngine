{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Evaluate where

import Fen
import Move
import Utils

startBoard = (\(Right x) -> x) $ parseBoard startFen

nextBoards board =
  concatMap (`makeMove` board) $ generatePseudoLegalMoves board

allNextBoards board depth = f board 0
  where
    f board n
      | n == depth = []
      | otherwise =
          concatMap (\b -> f b (n + 1)) (nextBoards board) ++ nextBoards board
