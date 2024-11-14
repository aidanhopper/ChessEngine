const API_URL = "http://localhost:3001"

export const possibleMoves = async (fen: string) => {
  const url = `${API_URL}/possible-moves?fen=${encodeURIComponent(fen)}`

  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
    headers: {
      'Content-Type': 'text/plain',
    },
  })
    .then(response => response.json())

  return response;
}

export const makeMove = async (fen: string, start: string, target: string) => {
  const url = `${API_URL}/make-move?fen=${encodeURIComponent(fen)}&start=${start}&target=${target}`

  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
    headers: {
      'Content-Type': 'text/plain',
    },
  })
    .then(response => response.json())

  return response;
}

