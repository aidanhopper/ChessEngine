{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Endpoints where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Text.Lazy (Text, pack)
import Data.Text.Lazy qualified as TL
import Debug.Trace
import Evaluate
import Fen
import GHC.Generics
import Move as Chess
import Utils
import Web.Scotty

data MoveRequest = MoveRequest
  { fen :: String,
    move :: String
  }
  deriving (Show, Generic)

data MoveResponse = MoveResponse
  { startingSquare :: String,
    targetSquare :: String
  }
  deriving (Show, Generic)

instance ToJSON MoveRequest

instance FromJSON MoveRequest

instance ToJSON MoveResponse

instance FromJSON MoveResponse

serve :: IO ()
serve = do
  scotty 3000 $ do
    get "/possible-moves" $ do
      fen <- queryParam "fen"
      case Chess.generatePseudoLegalMoves <$> parseBoard fen of
        Right moves ->
          json $
            map
              (\(Move s t _) -> MoveResponse (convertIndex s) (convertIndex t))
              moves
        Left err -> json err
