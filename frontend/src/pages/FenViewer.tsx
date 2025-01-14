import Game from '../Game';
import { useState, useRef, useEffect } from 'react';
import { possibleMoves, makeMove } from '../Query';

const startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

const FenViewer = () => {

  const calcTileSize = () =>
    Math.min(window.innerWidth, window.innerHeight) / 12;

  const [tileSize, setTileSize] = useState(calcTileSize());
  const [fen, setFen] = useState(startFen);
  const [validMoves, setValidMoves] = useState([]);

  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (inputRef.current) {
      inputRef.current.value = startFen;
    }

    possibleMoves(startFen).then(res => {
      if (res.ok) {
        setValidMoves(res.body);
      }
    });
  }, [])

  window.addEventListener("resize", () => {
    setTileSize(calcTileSize());
  });

  const onMove = (start: string, target: string) => {
    makeMove(fen, start, target).then(res => {
      console.log(res);
      if (res.ok) {
        const newFen = res.body[0]

        setFen(newFen);
        if (inputRef.current) {
          inputRef.current.value = newFen;
        }

        possibleMoves(newFen).then(res => {
          if (res.ok) {
            setValidMoves(res.body);
          }
        })
      }
    });
  }

  return (
    <div className="m-auto">
      <Game
        tileSize={tileSize}
        onBlackMove={onMove}
        onWhiteMove={onMove}
        disabledSides=""
        fen={fen}
        validMoves={validMoves}
        lastMove={[]}
      />
      <div className="flex mt-2">
        <input
          className="mt-2 p-2 flex-auto"
          type="text"
          placeholder="Fen"
          ref={inputRef}
        />
        <button
          className="mt-2 p-2 text-white bg-gray-700 hover:bg-gray-500 duration-100"
          onClick={() => {
            if (inputRef.current) {
              setFen(inputRef.current?.value);
            }
          }}
        >
          Set
        </button>
      </div>
    </div>
  );
}

export default FenViewer;
