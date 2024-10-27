{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import Debug.Trace
import Evaluate
import Fen
import Move
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Utils

type API =
  "moves" :> Capture "fen" String :> Capture "square" String :> Get '[JSON] [String]
    :<|> "domove" :> Capture "fen" String :> Capture "moving" String :> Capture "target" String :> Get '[JSON] (Maybe [String])
    :<|> "bestmove" :> Capture "fen" String :> Get '[JSON] String
    :<|> "startstring" :> Get '[JSON] String

server :: Server API
server =
  handleMoves
    :<|> handleDoMove
    :<|> handleBestMove
    :<|> handleStartString
  where
    handleMoves :: String -> String -> Handler [String]
    handleMoves fenString square = return $ trace ("Possible moves: " ++ show out) out
      where
        out =
          map (\(Algebraic x) -> x) $
            generateSafeMoves (Algebraic square) fenString

    handleDoMove :: String -> String -> String -> Handler (Maybe [String])
    handleDoMove fenString moving target =
      if Algebraic target `elem` possibleMoves
        then
          return $
            Just $
              trace
              ("Doing move " ++ moving ++ " to " ++ target ++ " " ++ show move) move
        else return Nothing
      where
        possibleMoves =
          generateSafeMoves (Algebraic moving) fenString
        move = doMove (Algebraic moving) (Algebraic target) fenString

    handleBestMove :: String -> Handler String
    handleBestMove fenString = return $ trace ("Getting the best moves fen string " ++ show out) out
      where
        out = generateBestMove fenString

    handleStartString :: Handler String
    handleStartString = return $ trace (show out) out
      where
        out = "The start string: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

app :: Application
app = serve (Proxy :: Proxy API) server

runServer :: IO ()
runServer = do
  putStrLn "Running server on http://localhost:8080"
  run 8080 app
