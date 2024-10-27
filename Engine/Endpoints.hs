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
