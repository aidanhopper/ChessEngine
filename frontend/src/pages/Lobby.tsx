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
      isCheckMate: false,
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
  const tileSize = Math.min(window.innerWidth, window.innerHeight) / 12;

  const w = useMemo<WebSocket>(() => {
    const w = new WebSocket(`ws://localhost:4000/ws?session=${getSessionId()}`);

    w.onmessage = (event) => {
      const data = JSON.parse(event.data)
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

  const disabledSides =
    gameState === undefined || !gameState.isMyTurn || !gameState.isStarted ?
      "wb" :
      sideToMove(gameState.fen) === "w" ?
        "b" : "w"

  return (
    <div className="flex flex-col text-center content-center bg-gray-50 h-screen w-screen -mt-20">
      {
        !gameState.isStarted
        && (isLobbyCreator ?
          <div className="sm:text-4xl mx-auto content-center text-center h-full w-full">
            <div className="m-10 text-2xl">
              Give this link to a friend
            </div>
            <code className="bg-gray-100 p-5 rounded">
              https://chess.ahop.dev/{lobby}
            </code>
          </div> :
          <div>
            <button className="border rounded border-black p-2 hover:bg-black
            hover:text-white transition-colors duration-100 ease-in-out my-[25%]"
              onClick={() => {
                if (lobby) {
                  sendRegisterMessage(w, lobby)
                }
              }}
            >
              Join?
            </button>
          </div>)
      }
      {
        gameState !== undefined
        && gameState.isStarted
        && gameState.isCheckMate
        &&
        <div className="text-center mt-32 text-3xl">
          CHECK MATE
          <div className="font-bold">
            {
              sideToMove(gameState.fen) === "w" ? "black wins" : "white wins"
            }
          </div>
        </div>
      }
      {
        gameState !== undefined
        && gameState.isStarted
        &&
        <>
          <div className="flex-auto content-center">
            <div className="rounded-lg mx-auto border-8 border-black max-h-fit max-w-fit">
              <Game
                onWhiteMove={onMove}
                onBlackMove={onMove}
                tileSize={tileSize}
                disabledSides={disabledSides}
                fen={gameState.fen}
                lastMove={gameState.lastMove}
                validMoves={gameState.possibleMoves}
              />
            </div>
          </div>
        </>
      }
    </div >
  );
}

export default Lobby;
