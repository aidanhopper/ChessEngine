export type Fen = {
  piecePlacement: string,
  sideToMove: string,
  castlingAbility: string,
  enPassantTargetSquare: string,
  halfmoveClock: number,
  fullmoveClock: number,
};

const parseFen = (fenstring: string): Fen => {
  const splitString = fenstring.split(" ");

  const parsedFen: Fen = {
    piecePlacement: splitString[0],
    sideToMove: splitString[1],
    castlingAbility: splitString[2],
    enPassantTargetSquare: splitString[3],
    halfmoveClock: Number(splitString[4]),
    fullmoveClock: Number(splitString[5]),
  };

  return parsedFen;
}

export default parseFen;
