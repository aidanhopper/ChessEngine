{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Endpoints where

import Data.Aeson (ToJSON)
import Data.Text (Text, pack)
import Debug.Trace
import Move
import Fen
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Utils

type API =
  "moves" :> Capture "fen" String :> Capture "square" String :> Get '[JSON] [String]
    :<|> "domove" :> Capture "fen" String :> Capture "moving" String :> Capture "target" String :> Get '[JSON] (Maybe [String])

server :: Server API
server =
  handleMoves
    :<|> handleDoMove
  where
    handleMoves :: String -> String -> Handler [String]
    handleMoves fenString square =
      return $
        map (\(Algebraic x) -> x) $
          generateSafeMoves (Algebraic square) fenString

    handleDoMove :: String -> String -> String -> Handler (Maybe [String])
    handleDoMove fenString moving target =
      if Algebraic target `elem` possibleMoves
        then return $ Just $ trace (show $ doMove (Algebraic moving) (Algebraic target) fenString) $ doMove (Algebraic moving) (Algebraic target) fenString
        else return Nothing
      where
        possibleMoves =
          generateSafeMoves (Algebraic moving) fenString

app :: Application
app = serve (Proxy :: Proxy API) server

runAPI :: IO ()
runAPI = do
  putStrLn "Running server on http://localhost:8080"
  run 8080 app
