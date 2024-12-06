import { useState } from 'react';
import Draggable, { DraggableEventHandler } from 'react-draggable';
import { toPos } from './Utils';

type PieceProps = {
  imagePath: string;
  tileSize: number;
  index: number;
  hoverIndex: number;
  isValidMove: (start: number) => boolean;
  disabled?: boolean,
  onPickup?: (e: MouseEvent | TouchEvent, index: number) => void;
  onPutdown?: (e: MouseEvent | TouchEvent, data: { x: number; y: number }, index: number) => void;
  onDrag?: (e: MouseEvent | TouchEvent, data: { x: number; y: number }) => void;
}

const Piece = ({ imagePath, tileSize, index, hoverIndex, disabled, isValidMove, onPickup, onPutdown, onDrag }: PieceProps) => {

  const defaultPosition = toPos(index, tileSize);
  const [position, setPosition] = useState(defaultPosition);
  const [z, setZ] = useState(0);

  const handlePickup: DraggableEventHandler = (e) => {
    if (onPickup) {
      onPickup(e as MouseEvent | TouchEvent, index);
    }
    setZ(1);
  }

  const handlePutdown: DraggableEventHandler = (e, data) => {
    if (onPutdown) {
      onPutdown(e as MouseEvent | TouchEvent, { x: data.x, y: data.y }, index)
    }

    setZ(0)

    if (isValidMove(index)) {
      setPosition(toPos(hoverIndex, tileSize));
    }
    else {
      setPosition(defaultPosition)
    }
  }

  const handleDrag: DraggableEventHandler = (e, data) => {
    if (onDrag) {
      onDrag(e as MouseEvent | TouchEvent, data);
    }
    setPosition({ x: data.x, y: data.y })
  }

  return (
    <Draggable
      disabled={disabled}
      onStart={handlePickup}
      onStop={handlePutdown}
      onDrag={handleDrag}
      bounds="parent"
      position={position}
      defaultPosition={defaultPosition}>
      <div className="absolute select-none"
        style={{
          width: `${tileSize}px`,
          height: `${tileSize}px`,
          zIndex: z,
        }}>
        <img
          draggable="false"
          className="pointer-events-none select-none w-full"
          src={imagePath} alt="Piece"
          style={{
            width: `${tileSize}px`
          }}
        />
      </div>
    </ Draggable>
  );
}

export default Piece;
