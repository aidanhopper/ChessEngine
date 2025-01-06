import Game from '../Game';
import { useState, useRef, useEffect } from 'react';

const startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

const FenViewer = () => {

  const calcTileSize = () =>
    Math.min(window.innerWidth, window.innerHeight) / 12;

  const [tileSize, setTileSize] = useState(calcTileSize());
  const [fen, setFen] = useState(startFen);
  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    if (inputRef.current) {
      inputRef.current.value = startFen;
    }
  }, [])

  window.addEventListener("resize", () => {
    setTileSize(calcTileSize());
  });

  const onMove = (start: string, end: string) => {

  }

  return (
    <div className="m-auto">
      <Game
        tileSize={tileSize}
        onBlackMove={onMove}
        onWhiteMove={onMove}
        disabledSides=""
        fen={fen}
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
            console.log(inputRef.current?.value);
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
