const ENGINE_URL = "http://localhost:3001"
const BACKEND_URL = "http://localhost:4000";

export const possibleMoves = async (fen: string) => {
  const url = `${ENGINE_URL}/possible-moves?fen=${encodeURIComponent(fen)}`

  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}

export const makeMove = async (fen: string, start: string, target: string) => {
  const url = `${ENGINE_URL}/make-move?fen=${encodeURIComponent(fen)}&start=${start}&target=${target}`

  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json())

  return response;
}

export const createLobby = async () => {
  const response = await fetch(`${BACKEND_URL}/api/v1/create-lobby`, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json())

  return response;
}

export const isLobbyAlive = async (lobby: string | undefined) => {
  if (lobby === undefined) {
    return false;
  }

  const url = `${BACKEND_URL}/api/v1/is-lobby-alive?lobby=${lobby}`
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response
}

export const present = async (lobby: string, sessionId: string) => {
  const url = `${BACKEND_URL}/api/v1/present?lobby=${lobby}&session=${sessionId}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response
}
