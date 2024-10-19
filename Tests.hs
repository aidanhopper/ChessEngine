module Tests where

import Evaluate
import Fen
import Move
import Test.HUnit
import Utils

testPawnMoveGenerator =
  TestCase
    ( do
        let fen = "rnbqkbnr/ppp1p1pp/8/2Pp4/1P2P3/3P1p2/P4PPP/RNBQKBNR w KQkq d6 0 1"

        -- Test enPassant
        assertEqual
          "En passant move generation failed."
          (generateSafeMoves (Algebraic "c5") fen)
          [Algebraic "d6", Algebraic "c6"]

        assertBool
          "Pawn attack left failed." $
          Algebraic "d5" `elem` generateSafeMoves (Algebraic "e4") fen

        assertBool
          "Pawn move forward failed." $
          Algebraic "b5" `elem` generateSafeMoves (Algebraic "b4") fen
    )
