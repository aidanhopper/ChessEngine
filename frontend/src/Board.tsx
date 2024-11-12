import React from 'react';
import Draggable from 'react-draggable';

import { Fen } from './Fen'
import parseFen from './Fen'

type BoardProps = {
  fenString: string;
  tileSize: number;
  color1: string;
  color2: string;
}

type PieceProps = {
  imagePath: string;
  tileSize: number;
  index: number;
}

const Piece = ({ imagePath, tileSize, index }: PieceProps) => {
  const posX = (index % 8) * tileSize;
  const posY = (Math.floor(index / 8)) * tileSize;

  return (
    <Draggable bounds="parent" defaultPosition={{ x: posX, y: posY }}>
      <div className="absolute"
        style={{
          transform: `translate(${posX}px, ${posY}px)`,
        }}
      >
        <img className="pointer-events-none select-none" src={imagePath} alt="Piece" style={{ width: `${tileSize}px` }} />
      </div>
    </ Draggable>
  );
}

const Board = ({ fenString, tileSize, color1, color2 }: BoardProps) => {
  const f: Fen = parseFen(fenString);

  const squares: string[] = []

  // Populate the squares list
  f.piecePlacement.split("/").forEach((row) => {
    row.split('').forEach((char: string) => {
      if (!isNaN(Number(char))) {
        for (let i = 0; i < Number(char); i++) {
          squares.push(' ');
        }
      }
      else {
        squares.push(char);
      }
    });
  });

  const s = {
    display: 'flex',
    width: `${tileSize}px`,
    height: `${tileSize}px`
  };

  const Row1 = () => <>
    <div className="flex flex-row">
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
    </div>
  </>

  const Row2 = () => <>
    <div className="flex flex-row">
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
      <span style={{ ...s, backgroundColor: color2 }} />
      <span style={{ ...s, backgroundColor: color1 }} />
    </div>
  </>

  return (
    <div className="flex-col bg-black inline-flex">
      {
        squares.map((piece, index) => {
          if (piece !== ' ') {
            const imagePath = piece.toLowerCase() === piece ?
              `/assets/${piece}b.png` :
              `/assets/${piece.toLowerCase()}w.png`;

            return (
              <Piece key={index} imagePath={imagePath} tileSize={tileSize} index={index} />
            )
          }

          return <></>
        })
      }
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
      <Row1 />
      <Row2 />
    </div>
  );
}

export default Board;
