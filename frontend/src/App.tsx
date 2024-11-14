import { useState, useEffect } from 'react';
import Board from './Board';
import { makeMove, possibleMoves } from './Query';

const startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

const App = () => {

  const [fen, setFen] = useState<string>(startFen);
  const [moves, setMoves] = useState<{ startingSquare: string, targetSquare: string }[]>([]);

  useEffect(() => {
    possibleMoves(startFen)
      .then(res => {
        setMoves(res);
      })
  }, [])

  console.log(moves);

  return (
    <>
      <Board
        fenString={fen}
        onMove={(start, target) => {
          makeMove(fen, start, target).then(res => {
            if (res !== "Move does not exist") {
              setFen(res)
              possibleMoves(res).then(m => setMoves(m));
            }
          });
        }}
        validMoves={moves}
        tileSize={100}
        color1="#769656"
        color2="#eeeed2" />
    </>
  );
}

export default App;
