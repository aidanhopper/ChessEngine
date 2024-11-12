import React from 'react';
import Board from './Board'


const App = () => {

  const startFen = "rn3b1r/p2ppppp/q1b5/5kn1/1p1p4/4P1P1/P1PPP2P/RNBQKBNR b KQkq - 0 1";
  return (
    <>
      <Board
        fenString={startFen}
        tileSize={100}
        color1={"#769656"}
        color2={"#eeeed2"} />
    </>
  );
}

export default App;
