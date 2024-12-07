import { useParams } from 'react-router-dom';
import { present } from '../Query';
import { useNavigate, useLocation } from 'react-router-dom';
import { v4 as uuid } from 'uuid';
import { useState, useEffect, useMemo } from 'react';
import { sideToMove } from '../Utils'
import Game from '../Game';
import { playMoveSound, playCaptureSound } from '../Audio';

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
      soundToPlay: "",
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

const sendPresence = (lobby: string | undefined, w: WebSocket) => {
  if (lobby === undefined) {
    return false
  }

  present(lobby, getSessionId()).then(res => {
    console.log(res)
    if (!res.ok) {
      return false
    }
    waitForWebSocketConnection(() => {
      console.log("SENDING REGISTER MESSAGE")
      sendRegisterMessage(w, lobby);
    }, w);
  });

  return true
}

const Lobby = () => {
  const navigate = useNavigate();
  const location = useLocation();
  const isLobbyCreator = location.state === "creator";
  const { lobby } = useParams();
  const [gameState, setGameState] = useState(getCachedGameState(lobby));

  const calcTileSize = () => Math.min(window.innerWidth, window.innerHeight) / 12;

  const [tileSize, setTileSize] = useState(calcTileSize());

  window.addEventListener("resize", () => {
    setTileSize(calcTileSize());
  });

  const w = useMemo<WebSocket>(() => {
    const w = new WebSocket(`/api/v1/ws?session=${getSessionId()}`);

    w.onmessage = (event) => {
      const data = JSON.parse(event.data);
      setGameState(data);
      if (lobby !== undefined) {
        setCachedGameState(lobby, data);
      }
    }

    return w;
  }, [lobby])

  useEffect(() => {
    switch (gameState.soundToPlay) {
      case "": break;
      case "move": playMoveSound(); break;
      case "capture": playCaptureSound(); break;
    }
  }, [gameState]);

  useEffect(() => {
    if (isLobbyCreator) {
      if (!sendPresence(lobby, w)) {
        navigate("/page-not-found");
      }
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
    <div className="flex flex-col flex-auto text-center content-center  w-screen">
      {
        !gameState.isStarted
        && (isLobbyCreator ?
          <div className="flex-auto sm:text-4xl mx-auto content-center h-full w-full">
            <div className="inline-block flex-row bg-gray-100 rounded p-4 h-fill">
              <div className="flex-auto text-left text-gray-400 text-sm mb-1">
                Send this to a friend
              </div>
              <code className="flex-auto content-center">
                https://chess.ahop.dev/{lobby}
              </code>
            </div>
          </div> :
          <div className="flex-auto content-center h-full w-fill m-auto">
            <button className="flex border rounded border-black p-2 hover:bg-black
            hover:text-white duration-100 ease-in-out content-center"
              onClick={() => {
                if (!sendPresence(lobby, w)) {
                  navigate("/page-not-found")
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
        <div className="text-center text-3xl">
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
            <div className="rounded-lg mx-auto border-black max-h-fit max-w-fit">
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
