import parseFen from './Fen'

export const toPosition = (index: number) => {
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

export const fromPosition = (pos: string) => {
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

export const sideToMove = (fen: string) => {
  return parseFen(fen).sideToMove;
}

export const toPos = (index: number, tileSize: number) => {
  const posX = (index % 8) * tileSize;
  const posY = (Math.floor(index / 8)) * tileSize;
  return { x: posX, y: posY };
}
