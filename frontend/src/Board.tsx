import { useState } from 'react';
import Piece from './Piece'


import { Fen } from './Fen'
import parseFen from './Fen'

const playMoveSound = () => {
  new Audio('/sounds/move.mp3')
    .play()
    .catch(error => console.error('Move sound failed: ', error));
}

const playCaptureSound = () => {
  new Audio('/sounds/capture.mp3')
    .play()
    .catch(error => console.error('Capture sound failed: ', error));
}

const fromPosition = (pos: string) => {
  if (pos.length !== 2) {
    throw new Error("Input position is invalid.");
  }

  const colChar = pos[0];
  const rowChar = pos[1];

  let col = -1;
  switch (colChar) {
    case 'a':
      col = 0;
      break;
    case 'b':
      col = 1;
      break;
    case 'c':
      col = 2;
      break;
    case 'd':
      col = 3;
      break;
    case 'e':
      col = 4;
      break;
    case 'f':
      col = 5;
      break;
    case 'g':
      col = 6;
      break;
    case 'h':
      col = 7;
      break;
  }

  let row = -1;
  switch (rowChar) {
    case '1':
      row = 7;
      break;
    case '2':
      row = 6;
      break;
    case '3':
      row = 5;
      break;
    case '4':
      row = 4;
      break;
    case '5':
      row = 3;
      break;
    case '6':
      row = 2;
      break;
    case '7':
      row = 1;
      break;
    case '8':
      row = 0;
      break;
  }

  if (row === -1 || col === -1) {
    throw new Error("Input position is invalid.");
  }

  return (row * 8) + col
}

const toPosition = (index: number) => {
  if (index < 0 || index > 63) {
    throw new Error("Input index is invalid.");
  }

  let col = '';
  switch (index % 8) {
    case 0:
      col = 'a';
      break;
    case 1:
      col = 'b'
      break;
    case 2:
      col = 'c';
      break;
    case 3:
      col = 'd';
      break;
    case 4:
      col = 'e';
      break;
    case 5:
      col = 'f';
      break;
    case 6:
      col = 'g';
      break;
    case 7:
      col = 'h';
      break;
  }

  let row = '';
  switch (Math.floor(index / 8)) {
    case 0:
      row = '8';
      break;
    case 1:
      row = '7'
      break;
    case 2:
      row = '6';
      break;
    case 3:
      row = '5';
      break;
    case 4:
      row = '4';
      break;
    case 5:
      row = '3';
      break;
    case 6:
      row = '2';
      break;
    case 7:
      row = '1';
      break;
  }

  return col + row;
}

type BoardProps = {
  fenString: string;
  tileSize: number;
  color1: string;
  color2: string;
  validMoves?: { startingSquare: string, targetSquare: string, isCapture: boolean }[];
  onMove?: (start: string, target: string) => void;
  disabledSides: string;
}

const Board = ({ fenString, tileSize, color1, color2, validMoves, onMove, disabledSides }: BoardProps) => {

  const [hoverIndex, setHoverIndex] = useState(-1);
  const [pickupIndex, setPickupIndex] = useState(-1);

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
    <div className="flex-col absolute ">
      {
        squares.map((piece, index) => {
          if (piece !== ' ') {
            const side = piece.toLowerCase() === piece ? "b" : "w";
            const imagePath = `/assets/${piece}${side}.png`;
            const pieceIsDisabled =
              disabledSides.split('').filter(e => side === e)
                .length !== 0;

            return (
              <Piece
                key={index}
                disabled={pieceIsDisabled}
                hoverIndex={hoverIndex}
                onPickup={(e, index) => {
                  setHoverIndex(index);
                  setPickupIndex(index);
                }}
                onPutdown={(e, data, index) => {
                  const start = toPosition(index);
                  const target = toPosition(hoverIndex);

                  if (onMove) {
                    onMove(start, target);
                  }

                  setHoverIndex(-1);
                  setPickupIndex(-1);

                  if (validMoves) {
                    validMoves.forEach(elem => {
                      if (elem.startingSquare === start &&
                        elem.targetSquare === target) {
                        if (elem.isCapture) {
                          playCaptureSound();
                        }
                        else {
                          playMoveSound();
                        }
                      }
                    })
                  }
                }}
                onDrag={(e, data) => {
                  const _hoverIndex =
                    Math.floor((data.x + (tileSize / 2)) / tileSize) +
                    (Math.floor((data.y + (tileSize / 2)) / tileSize) * 8);
                  setHoverIndex(_hoverIndex);
                }}
                isValidMove={startingIndex => {
                  const start = toPosition(startingIndex);
                  const target = toPosition(hoverIndex);

                  if (validMoves) {
                    let isValid = false;
                    validMoves.forEach(elem => {
                      if (elem.startingSquare === start &&
                        elem.targetSquare === target) {
                        isValid = true;
                      }
                    })
                    return isValid;
                  }

                  return true;
                }}
                imagePath={imagePath}
                tileSize={tileSize}
                index={index} />
            );
          }

          return <></>
        })
      }
      {
        validMoves !== undefined && pickupIndex !== -1 &&
        validMoves.map(elem => {
          const startIndex = fromPosition(elem.startingSquare);
          if (startIndex === pickupIndex) {
            const sqSize = tileSize / 3;
            const targetIndex = fromPosition(elem.targetSquare);
            const posX = (targetIndex % 8) * tileSize +
              Math.floor(tileSize / 2 - sqSize / 2);
            const posY = (Math.floor(targetIndex / 8)) * tileSize +
              Math.floor(tileSize / 2 - sqSize / 2);
            return (
              <div
                className="absolute bg-green-700 rounded-full"
                style={{
                  transform: `translate(${posX}px, ${posY}px)`,
                  width: `${Math.floor(sqSize)}px`,
                  height: `${Math.floor(sqSize)}px`,
                  boxShadow: `0px 0px 5px rgba(0, 0, 0, 0.4)`,
                }}>
              </div>
            );
          }
          return <></>
        })
      }
      {
        hoverIndex !== -1 &&
        <div
          className="absolute border-blue-700"
          style={{
            width: `${tileSize}px`,
            height: `${tileSize}px`,
            transform: `translate(${(hoverIndex % 8) * tileSize}px, ${(Math.floor(hoverIndex / 8)) * tileSize}px)`,
            borderWidth: `${tileSize / 12}px`
          }}>
        </div>
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
