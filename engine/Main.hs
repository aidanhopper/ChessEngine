{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Bits
import Data.Text.Lazy (Text, pack)
import Debug.Trace
import Evaluate
import Fen
import GHC.Generics
import Move as Chess
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors
import Numeric (showIntAtBase)
import Utils
import Web.Scotty

f = "1nbqkbnr/2p2p2/6q1/3B4/8/3Q4/8/3K4 w KQkq - 0 1"

printBitboard :: Bitboard -> IO ()
printBitboard bb = do
  putStrLn $ showPrettyBitboard bb ++ "\n"

printBitboards :: [Bitboard] -> IO ()
printBitboards [] = return ()
printBitboards (bb : bbs) = do
  printBitboard bb
  printBitboards bbs

data MoveRequest = MoveRequest
  { fen :: String,
    start :: String,
    target :: String
  }
  deriving (Show, Generic)

data MoveResponse = MoveResponse
  { startingSquare :: String,
    targetSquare :: String,
    isCapture :: Bool
  }
  deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

makeMoveFen :: String -> String -> String -> Either String [String]
makeMoveFen f s t = do
  board <- parseBoard f
  let moves = generateMoves board
  move <- getMove s t moves
  let newBoards = makeMove move board
  return $ map boardToFenString newBoards

main :: IO ()
main = scotty 3001 $ do
  middleware simpleCors

  get "/hello" $ do
    text "Hello"

  get "/possible-moves" $ do
    fen <- queryParam "fen"
    let board = parseBoard fen
    case Chess.generateMoves <$> board of
      Right moves ->
        json $
          map
            ( \(Move s t f) ->
                MoveResponse
                  (convertIndex s)
                  (convertIndex t)
                  (Utils.isCapture f)
            )
            moves
      Left err -> json err

  get "/make-move" $ do
    fen <- queryParam "fen"
    startingSqr <- queryParam "start"
    targetSqr <- queryParam "target"
    case makeMoveFen fen startingSqr targetSqr of
      Right newFen -> json newFen
      Left err -> json err
