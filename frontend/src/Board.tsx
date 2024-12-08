import { useState } from 'react';
import Piece from './Piece';
import { toPosition, fromPosition } from './Utils';
import { Fen } from './Fen'
import parseFen from './Fen'
import { playMoveSound, playCaptureSound } from './Audio';
import { toPos } from './Utils';

const Arrow = ({ size, startIndex, targetIndex }:
  { size: number, startIndex: number, targetIndex: number }) => {
  const startPos = toPos(startIndex, size);
  const targetPos = toPos(targetIndex, size);
  const origin = { x: size / 6, y: size / 2, };
  const start = { x: startPos.x / size, y: startPos.y / size };
  const target = { x: targetPos.x / size, y: targetPos.y / size };
  const diff = { x: target.x - start.x, y: target.y - start.y };
  const h = Math.sqrt(diff.x * diff.x + diff.y * diff.y);
  const length = Math.sqrt(
    (targetPos.x - startPos.x) * (targetPos.x - startPos.x) +
    (targetPos.y - startPos.y) * (targetPos.y - startPos.y)
  );

  const rot = () => {
    const theta = Math.asin(diff.y / h);
    let phi = Math.PI - theta;
    if (diff.x <= 0) {
      phi = theta;
    }
    return phi;
  }

  return (
    <>
      <div className="absolute"
        style={{
          transform: `translate(${origin.x + targetPos.x}px, ${origin.y + targetPos.y}px)`,
          zIndex: 10,
        }}>
        <div className="opacity-80 absolute"
          style={{
            transform: `rotate(${-rot() - Math.PI / 2}rad)`,
            transformOrigin: `${size / 3}px 0px`,
          }}>
          <div className="absolute"
            style={{
              borderWidth: `0 ${size / 3}px ${size / 3 + size / 20}px ${size / 3}px`,
              borderColor: `transparent transparent orange transparent`,
            }}>
          </div>
          <div className="absolute"
            style={{
              width: `${size / 4}px`,
              height: `${length - (size * 0.6)}px`,
              background: 'orange',
              transform: `translate(${size / 5}px, ${size / 10 + size / 8}px)`
            }}>
          </div>
        </div>
      </div>
    </>
  );
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
  const [arrows, setArrows] = useState<number[][]>([]);
  const [arrowStartIndex, setArrowStartIndex] = useState(-1);
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

  const calcIndexFromEvent = (e: any) => {
    const { left, top } = e.currentTarget.getBoundingClientRect();
    const clickPos = {
      x: Math.floor((e.clientX - left) / tileSize),
      y: Math.floor((e.clientY - top) / tileSize)
    };
    return clickPos.y * 8 + clickPos.x;
  };

  const handleMouseDown = (e: any) => {
    if (e.button === 0) {
      setArrows([]);
    } else if (e.button === 2) {
      setArrowStartIndex(calcIndexFromEvent(e));
    }
  };

  const handleMouseUp = (e: any) => {
    if (e.button !== 2) {
      return;
    }

    const start = arrowStartIndex;
    const end = calcIndexFromEvent(e);

    if (start !== -1 && start !== end) {
      arrows.push([start, end])
      setArrows([...arrows]);
    }
  };

  const handleMouseLeave = () => {
    setArrowStartIndex(-1);
  };

  return (
    <div className="flex-col absolute"
      onContextMenu={(e: any) => e.preventDefault()}
      onMouseDown={handleMouseDown}
      onMouseUp={handleMouseUp}
      onMouseLeave={handleMouseLeave}>
      {
        arrows.map(idxs => <>
          <Arrow size={tileSize} startIndex={idxs[0]} targetIndex={idxs[1]} />
        </>
        )
      }
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
            const imagePath = `/assets/${piece.toLowerCase()}${side}.png`;
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

                  if (!validMoves) {
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
      <div className=""
      style={{
          outline: `black solid ${tileSize/10}px`,
          borderRadius: `${tileSize/15}px`,
        }}>
        <Row1 />
        <Row2 />
        <Row1 />
        <Row2 />
        <Row1 />
        <Row2 />
        <Row1 />
        <Row2 />
      </div>
    </div>
  );
}

export default Board;
