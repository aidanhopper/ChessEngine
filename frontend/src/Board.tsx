import { useState } from 'react';
import Piece from './Piece';
import { toPosition, fromPosition } from './Utils';
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

type BoardProps = {
  fenString: string;
  tileSize: number;
  color1: string;
  color2: string;
  disabledSides?: string;
  validMoves?: {
    startingSquare: string;
    targetSquare: string;
    isCapture: boolean;
    isPawnPromotion: boolean;
  }[];
  onBlackMove?: (start: string, target: string) => void;
  onWhiteMove?: (start: string, target: string) => void;
  lastMove?: string[],
}

const Board = ({ fenString, tileSize, color1, color2, validMoves, onBlackMove, onWhiteMove, disabledSides, lastMove }: BoardProps) => {

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
    <div className="flex-col absolute">
      {
        lastMove && lastMove.length === 2 &&
        <>
          <div
            className="absolute bg-orange-500 opacity-30"
            style={{
              width: `${tileSize}px`,
              height: `${tileSize}px`,
              transform: `translate(
                ${(fromPosition(lastMove[0]) % 8) * tileSize}px,
                ${(Math.floor(fromPosition(lastMove[0]) / 8)) * tileSize}px)`,
            }}>
          </div>
          <div
            className="absolute bg-orange-500 opacity-30"
            style={{
              width: `${tileSize}px`,
              height: `${tileSize}px`,
              transform: `translate(
                ${(fromPosition(lastMove[1]) % 8) * tileSize}px,
                ${(Math.floor(fromPosition(lastMove[1]) / 8)) * tileSize}px)`,
            }}>
          </div>
        </>
      }
      {
        squares.map((piece, index) => {
          if (piece !== ' ') {
            const side = piece.toLowerCase() === piece ? "b" : "w";
            const imagePath = `/assets/${piece}${side}.png`;
            const pieceIsDisabled =
              disabledSides !== undefined ?
                disabledSides.split('').filter(e => side === e)
                  .length !== 0 : false;

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

                  if (side === "w" && onWhiteMove) {
                    onWhiteMove(start, target);
                  }

                  if (side === "b" && onBlackMove) {
                    onBlackMove(start, target);
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
                  } else {
                    playMoveSound();
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
