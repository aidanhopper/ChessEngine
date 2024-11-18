import Game from './Game'
import { useState } from 'react';

const App = () => {

  const tileSize = 100;

  const [channel, setChannel] = useState("");

  const ws = new WebSocket("ws://127.0.0.1:4001")

  ws.onopen = () => {
    ws.send('Hello from client!');
  }

  ws.onmessage = (event) => {
    console.log('Message from server ', event.data);
  }

  ws.onclose = () => {
    console.log('Disconnected from websocket server');
  }

  return (
    <div className="flex h-screen w-screen overflow-hidden">
      <div className="m-auto">
        <button
          onClick={() => {
            if (channel === "") {
              fetch("http://localhost:4000/api/v1/create-lobby", {
                method: 'GET',
                mode: 'cors',
                headers: {
                  'Content-Type': 'text/plain',
                },
              })
                .then(response => response.text())
                .then(text => setChannel(text))
            }

            ws.send(`My channel is ${channel}`);
          }}
          className="border rounded border-black p-2 hover:bg-black hover:text-white
          transition-colors duration-100 ease-in-out">
          Play with a friend!
        </button>
      </div>
    </div>
  )

}

export default App;
