{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Concurrent
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Database.PostgreSQL.Simple qualified as DB
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray (..))
import Debug.Trace
import GHC.Generics
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.WebSockets
import System.Random (randomRIO)
import Web.Scotty

data WebSocketMessage = WebSocketMessage
  { fenString :: String
  } deriving (Show)

websocketApp :: PendingConnection -> IO ()
websocketApp pending = do
  conn <- acceptRequest pending
  putStrLn "Client connected"
  forever $ do
    msg <- receiveData conn :: IO T.Text
    putStrLn $ "Recieved: " ++ show msg
    sendTextData conn $ encode $ object ["fen" .= startFen]
    return ()
  return ()

startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

data Lobby = Lobby
  { lobbyId :: String,
    fen :: String,
    playersPresent :: [String]
  }
  deriving (Show)

instance DB.FromRow Lobby where
  fromRow = do
    lobbyId <- field
    fen <- field
    PGArray playersPresent <- field
    return $ Lobby lobbyId fen (map T.unpack playersPresent)

defaultLobby = Lobby {lobbyId = "", fen = "", playersPresent = []}

connectToDb :: IO DB.Connection
connectToDb =
  DB.connect
    DB.defaultConnectInfo
      { DB.connectUser = "root",
        DB.connectPassword = "root",
        DB.connectDatabase = "postgres",
        DB.connectHost = "localhost"
      }

createLobby :: DB.Connection -> Lobby -> IO Int64
createLobby conn lobby = do
  DB.execute
    conn
    "INSERT INTO lobbies VALUES (?, ?, ?)"
    (lobbyId lobby, fen lobby, PGArray (playersPresent lobby))

getLobbyById :: DB.Connection -> String -> IO [Lobby]
getLobbyById conn lobbyId = do
  DB.query
    conn
    "SELECT * FROM lobbies WHERE lobbies.lobby_id = ?"
    (DB.Only lobbyId)

updateLobby :: DB.Connection -> Lobby -> IO Int64
updateLobby conn lobby = do
  DB.execute
    conn
    "UPDATE lobbies SET fen = ?, players_present = ? WHERE lobby_id = ?"
    (fen lobby, PGArray (playersPresent lobby), lobbyId lobby)

isLobbyAlive :: DB.Connection -> String -> IO Bool
isLobbyAlive conn lobby =
  case length lobby of
    6 -> do
      lobbies <- getLobbyById conn lobby
      case length lobbies of
        1 -> return True
        _ -> return False
    _ -> return False

createLobbyEndpoint = do
  get "/api/v1/create-lobby" $ do
    lobbyNum <- randomRIO (100000 :: Int, 999999 :: Int)
    let lobby = defaultLobby {lobbyId = show lobbyNum, fen = startFen}
    conn <- liftIO connectToDb
    _ <- liftIO $ createLobby conn lobby
    json $ lobbyId lobby

isLobbyAliveEndpoint =
  get "/api/v1/is-lobby-alive" $ do
    lobbyId <- queryParam "lobby" :: ActionM String
    conn <- liftIO connectToDb
    status <- liftIO $ isLobbyAlive conn lobbyId
    json status

presentEndpoint =
  get "/api/v1/present" $ do
    lobbyId <- queryParam "lobby" :: ActionM String
    sessionId <- queryParam "session" :: ActionM String
    conn <- liftIO connectToDb
    lobbies <- liftIO $ getLobbyById conn lobbyId
    case length lobbies of
      1 -> do
        let lobby = head lobbies
        let updatedPlayersPresent = sessionId : playersPresent lobby
        if sessionId `elem` playersPresent lobby
          then json (T.pack "Success")
          else
            if length (playersPresent lobby) == 2
              then json (T.pack "Lobby is full")
              else do
                _ <-
                  liftIO $
                    updateLobby conn $
                      lobby {playersPresent = updatedPlayersPresent}
                json (T.pack "Success")
      _ -> json (T.pack "Lobby does not exist")

restfulApp :: Int -> IO ()
restfulApp port = scotty port $ do
  middleware simpleCors
  createLobbyEndpoint
  isLobbyAliveEndpoint
  presentEndpoint

main :: IO ()
main = do
  _ <- forkIO $ restfulApp 4000
  putStrLn "Running websocket on port 4001"
  runServer "localhost" 4001 websocketApp
