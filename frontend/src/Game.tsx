import { useState, useEffect } from 'react';
import Board from './Board';
import parseFen from './Fen';
import { makeMove, possibleMoves } from './Query';

const sideToMove = (fen: string) => {
  return parseFen(fen).sideToMove;
}

const PromoPieceSelection = ({ side, tileSize, onSelect }:
  { side: string, tileSize: number, onSelect: (piece: string) => void }) => {
  return (
    <>
      <div
        className="absolute opacity-50 bg-white"
        style={{
          width: `${tileSize * 8}px`,
          height: `${tileSize * 8}px`
        }} />
      <div
        className="absolute content-center text-center"
        style={{
          width: `${tileSize * 8}px`,
          height: `${tileSize * 8}px`
        }}>
        <div className="flex flex-row ">
          <div
            className="flex-auto hover:scale-125 duration-100 ease-in-out transition-transform"
            onClick={() => onSelect('q')}>
            <img
              className="pointer-events-none m-auto"
              src={`/assets/q${side}.png`} alt="Piece"
              style={{ width: `${tileSize}px` }} />
          </div>
          <div
            className="flex-auto hover:scale-125 duration-100 ease-in-out transition-transform"
            onClick={() => onSelect('n')}>
            <img
              className="pointer-events-none m-auto"
              src={`/assets/n${side}.png`} alt="Piece"
              style={{ width: `${tileSize}px` }} />
          </div>
          <div
            className="flex-auto hover:scale-125 duration-100 ease-in-out transition-transform"
            onClick={() => onSelect('r')}>
            <img
              className="pointer-events-none m-auto"
              src={`/assets/r${side}.png`} alt="Piece"
              style={{ width: `${tileSize}px` }} />
          </div>
          <div
            className="flex-auto hover:scale-125 duration-100 ease-in-out transition-transform"
            onClick={() => onSelect('b')}>
            <img
              className="pointer-events-none m-auto"
              src={`/assets/b${side}.png`} alt="Piece"
              style={{ width: `${tileSize}px` }} />
          </div>
        </div>
      </div>
    </>
  );
}

type GameProps = {
  tileSize: number;
  onWhiteMove: (start: string, end: string) => void;
  onBlackMove: (start: string, end: string) => void;
  validMoves: {
    startingSquare: string;
    targetSquare: string;
    isCapture: boolean;
  }[];
  disabledSides: string;
  fen: string;
  className?: string;
}

const Game = ({
  tileSize,
  onWhiteMove,
  onBlackMove,
  validMoves,
  disabledSides,
  fen,
  className,
}: GameProps) => {

  const [sideToSelectPieceFor, setSideToSelectPieceFor] = useState("");

  return (
    <div className={`border-black ${className}`}
      style={{
        border: `${Math.floor(tileSize / 10)}px solid`,
        width: `${tileSize * 8 + Math.floor(tileSize / 5) - 2}px`,
        height: `${tileSize * 8 + Math.floor(tileSize / 5) - 2}px`,
      }}>
      <Board
        disabledSides={disabledSides}
        fenString={fen}
        onWhiteMove={onWhiteMove}
        onBlackMove={onBlackMove}
        validMoves={validMoves}
        tileSize={tileSize}
        color1="#769656"
        color2="#eeeed2" />
      {
        sideToSelectPieceFor !== "" &&
        <PromoPieceSelection
          side={sideToSelectPieceFor}
          tileSize={tileSize}
          onSelect={(piece) => {
            switch (piece) {
              case "q":
                //setFen(possibleFenStrings[0]);
                //possibleMoves(fen).then(m => setMoves(m));
                //setDisabledSides("");
                break;
              case "n":
                //setFen(possibleFenStrings[1]);
                //possibleMoves(fen[1]).then(m => setMoves(m));
                //setDisabledSides("");
                break;
              case "r":
                //setFen(possibleFenStrings[2]);
                //possibleMoves(possibleFenStrings[2]).then(m => setMoves(m));
                //setDisabledSides("");
                break;
              case "b":
                //setFen(possibleFenStrings[3]);
                //possibleMoves(possibleFenStrings[3]).then(m => setMoves(m));
                //setDisabledSides("");
                break;
            }
            setSideToSelectPieceFor("");
          }} />
      }
    </div>
  );
}

export default Game;
