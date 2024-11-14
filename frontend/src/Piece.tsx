import { useState } from 'react';
import Draggable, { DraggableEventHandler } from 'react-draggable';

type PieceProps = {
  imagePath: string;
  tileSize: number;
  index: number;
  onPickup?: (e: MouseEvent | TouchEvent, index: number) => void;
  onPutdown?: (e: MouseEvent | TouchEvent, data: { x: number; y: number }, index: number) => void;
  onDrag?: (e: MouseEvent | TouchEvent, data: { x: number; y: number }) => void;
}

const Piece = ({ imagePath, tileSize, index, onPickup, onPutdown, onDrag }: PieceProps) => {
  const posX = (index % 8) * tileSize;
  const posY = (Math.floor(index / 8)) * tileSize;

  const defaultPosition = { x: posX, y: posY };

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

    // Use timeout to stop piece flickering
    setTimeout(() => {
      setPosition(defaultPosition);
    }, 10)

  }

  const handleDrag: DraggableEventHandler = (e, data) => {
    if (onDrag) {
      onDrag(e as MouseEvent | TouchEvent, data);
    }
    setPosition({ x: data.x, y: data.y })
  }

  return (
    <Draggable
      onStart={handlePickup}
      onStop={handlePutdown}
      onDrag={handleDrag}
      bounds="parent"
      position={position}
      defaultPosition={defaultPosition}>
      <div className="absolute"
        style={{
          zIndex: z,
        }}>
        <img
          className="pointer-events-none select-none"
          src={imagePath} alt="Piece"
          style={{ width: `${tileSize}px` }} />
      </div>
    </ Draggable>
  );
}

export default Piece;
