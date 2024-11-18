import { useParams } from 'react-router-dom';
import { isLobbyAlive, present } from '../Query';
import { useNavigate } from 'react-router-dom';
import { v4 as uuid } from 'uuid';
import { useState } from 'react';

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

  const navigate = useNavigate();
  const { lobby } = useParams();
  const [started, setStarted] = useState(false);
  const w = new WebSocket(`ws://localhost:4001`);

  w.onmessage = (event) => {
    console.log(JSON.parse(event.data));
  }

  present(lobby, getSessionId()).then(res => {
    if (res !== "Success") {
      navigate("/page-not-found");
    }
    console.log(res);
    w.send("HERE :)");
  });

  return (
    <h1 className="text-6xl m-auto text-center h-screen content-center">
      {lobby}
    </h1>
  );
}

export default Lobby;
