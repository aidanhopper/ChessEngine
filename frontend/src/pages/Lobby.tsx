import { useParams } from 'react-router-dom';
import { present } from '../Query';
import { useNavigate, useLocation } from 'react-router-dom';
import { v4 as uuid } from 'uuid';
import { useState, useEffect, useMemo } from 'react';
import { sideToMove } from '../Utils'
import Game from '../Game';

const waitForWebSocketConnection = (callback: () => void, w: WebSocket) => {
  setTimeout(() => {
    if (w.readyState === 0) {
      waitForWebSocketConnection(callback, w);
    } else {
      if (callback) {
        callback();
      }
    }
  }, 10);
}

const getSessionId = () => {
  const idFromStorage = sessionStorage.getItem("ID_KEY");
  if (idFromStorage) {
    return idFromStorage;
  } else {
    const id = uuid(); // generate unique UUID
    sessionStorage.setItem("ID_KEY", id);
    return id;
  }
}

const getCachedGameState = (lobby: string | undefined) => {
  const state = sessionStorage.getItem("GAME_STATE");
  const newState = {
    lobby: lobby,
    body: {
      isStarted: false,
      fen: "",
      possibleMoves: [],
      isMyTurn: false,
      lastMove: [],
    }
  };

  if (lobby === undefined) {
    return newState.body;
  }

  if (state === null) {
    sessionStorage.setItem("GAME_STATE", JSON.stringify(newState));
    return newState.body;
  }

  const parsedState = JSON.parse(state);
  if (parsedState.lobby !== lobby) {
    sessionStorage.setItem("GAME_STATE", JSON.stringify(newState));
    return newState.body;
  }

  return parsedState.body;
}

const setCachedGameState = (lobby: string, gameState: any) => {
  const state = {
    lobby: lobby,
    body: gameState,
  }
  sessionStorage.setItem("GAME_STATE", JSON.stringify(state))
}

const sendRegisterMessage = (w: WebSocket, lobby: string) => {
  w.send(JSON.stringify({
    type: "register",
    lobby: lobby,
    move: [],
  }));
}

const Lobby = () => {
  const navigate = useNavigate();
  const location = useLocation();

  const isLobbyCreator = location.state === "creator";
  const { lobby } = useParams();
  const [gameState, setGameState] = useState(getCachedGameState(lobby));

  const w = useMemo<WebSocket>(() => {
    const w = new WebSocket(`ws://localhost:4000/ws?session=${getSessionId()}`);

    w.onmessage = (event) => {
      const data = JSON.parse(event.data)
      console.log(data)
      setGameState(data)
      if (lobby !== undefined) {
        setCachedGameState(lobby, data)
      }
    }

    return w;
  }, [lobby])

  useEffect(() => {
    if (lobby !== undefined) {
      present(lobby, getSessionId()).then(res => {
        if (!res.ok) {
          navigate("/page-not-found");
        }

        if (isLobbyCreator) {
          waitForWebSocketConnection(() => {
            sendRegisterMessage(w, lobby);
          }, w);
        }
      });
    } else {
      navigate("/page-not-found");
    }
  }, [lobby, navigate, w, isLobbyCreator])

  const onMove = (start: string, target: string) => {
    w.send(JSON.stringify({
      type: "makemove",
      lobby: lobby,
      move: [start, target],
    }));
  }

  const disabledSides = gameState === undefined || !gameState.isMyTurn || !gameState.isStarted ? "wb" :
    sideToMove(gameState.fen) === "w" ? "b" : "w"

  return (
    <h1 className="text-6xl m-auto text-center h-screen content-center">
      {
        !gameState.isStarted
        && (isLobbyCreator ?
          <>
            {lobby}
          </> :
          <>
            <button className="text-xl duration-100 transition-colors
                ease-in-out border-black rounded border-2 p-3 hover:bg-black
                hover:text-white"
              onClick={() => {
                if (lobby) {
                  sendRegisterMessage(w, lobby)
                }
              }}
            >
              Join?
            </button>
          </>)
      }
      {
        gameState !== undefined
        && gameState.isStarted
        && (gameState.isMyTurn ?
          <>YOUR TURN {sideToMove(gameState.fen)}</>
          : <>NOT YOUR TURN</>)
      }
      {
        gameState !== undefined
        && gameState.isStarted
        &&
        <>
          <Game
            className="m-auto"
            onWhiteMove={onMove}
            onBlackMove={onMove}
            tileSize={60}
            disabledSides={disabledSides}
            fen={gameState.fen}
            lastMove={gameState.lastMove}
            validMoves={gameState.possibleMoves}
          />
        </>
      }
    </h1 >
  );
}

export default Lobby;
