import { useParams } from 'react-router-dom';
import { present } from '../Query';
import { useNavigate } from 'react-router-dom';
import { v4 as uuid } from 'uuid';
import { useState, useEffect } from 'react';
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

const Lobby = () => {

  const { lobby } = useParams();
  const navigate = useNavigate();
  const [fen, setFen] = useState("")
  const [loaded, setLoaded] = useState(false)
  const w = new WebSocket(`ws://localhost:4000/ws?session=${getSessionId()}`);

  w.onmessage = (event) => {
    const data = JSON.parse(event.data)
    console.log(event.data)
    console.log(data);
    setFen(data.fen);
  }

  w.onopen = () => {
    console.log("connected to websocket");
  }

  w.onclose = () => {
    console.log("disconnected from websocket");
  }

  if (lobby !== undefined) {
    present(lobby, getSessionId()).then(res => {
      if (!res.ok) {
        w.close();
        navigate("/page-not-found");
      }

      setLoaded(true);

      waitForWebSocketConnection(() => {
        w.send(JSON.stringify({
          lobby: lobby,
        }));
      }, w);
    });
  } else {
    w.close();
    navigate("/page-not-found");
  }

  return (
    <h1 className="text-6xl m-auto text-center h-screen content-center">
      {
        !loaded &&
        <>
          Loading...
        </>
      }
      {
        fen === "" && loaded && lobby

      }
      {
        fen !== "" && loaded &&
        <>
          <Game
            className="m-auto"
            onWhiteMove={(start, target) => {

            }}
            onBlackMove={(start, target) => {

            }}
            tileSize={100}
            disabledSides=""
            fen={fen}
            validMoves={[]}
          />
        </>
      }
    </h1 >
  );
}

export default Lobby;
