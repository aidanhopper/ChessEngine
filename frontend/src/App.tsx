import { argv0 } from 'process';
import React from 'react';

type fen = {
  piecePlacement: string,
  sideToMove: string,
  castlingAbility: string,
  enPassantTargetSquare: string,
  halfmoveClock: number,
  fullmoveClock: number,
};

const parseFen = (fenstring: string): fen => {
  const splitString = fenstring.split(" ");

  const parsedFen: fen = {
    piecePlacement: splitString[0],
    sideToMove: splitString[1],
    castlingAbility: splitString[2],
    enPassantTargetSquare: splitString[3],
    halfmoveClock: Number(splitString[4]),
    fullmoveClock: Number(splitString[5]),
  };

  return parsedFen;
}

const Board = ({ fenstring }: { fenstring: string }) => {

  const f: fen = parseFen(fenstring);

  //const splitPiecePlacement: string[] = f.piecePlacement.split("/");

  const squares: string[] = []

  // Populate the squares list
  f.piecePlacement.split("/").forEach((row: string) => {
    let acc = 0;

    row.split('').forEach((char: string) => {
      if (!isNaN(Number(char))) {
        acc += Number(char);
      }

      else {
        for (let i = 0; i < acc; i++) {
          squares.push(' ');
        }

        squares.push(char);
      }

      if (acc === 8) {
        for (let i = 0; i < acc; i++) {
          squares.push(' ');
        }
      }
    });
  });

  const Row1 = () => (
    <div className="flex flex-row">
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
    </div>
  );

  const Row2 = () => (
    <div className="flex flex-row">
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
      <span className="w-20 h-20 bg-green-300"></span>
      <span className="w-20 h-20 bg-stone-300"></span>
    </div>
  );

  return (
    <>
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
    </>
  );
}

const App = () => {

  const startFen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

  return (
    <>
      <Board fenstring={startFen} />
    </>
  );
}

export default App;
