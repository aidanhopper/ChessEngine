export const createLobby = async (type: string) => {
  const response = await fetch(`/api/v1/create-lobby?type=${type}`, {
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

  const url = `/api/v1/is-lobby-alive?lobby=${lobby}`
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}

export const present = async (lobby: string, sessionId: string) => {
  const url = `/api/v1/present?lobby=${lobby}&session=${sessionId}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}

export const possibleMoves = async (fen: string) => {
  const url = `/api/v1/possible-moves?fen=${encodeURIComponent(fen)}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}

export const moveInfo = async (fen: string, start: string, target: string) => {
  const url = `/api/v1/move-info?fen=${encodeURIComponent(fen)}&start=${start}&target=${target}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}

export const makeMove = async (fen: string, start: string, target: string) => {
  const url = `/api/v1/make-move?fen=${encodeURIComponent(fen)}&start=${start}&target=${target}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response;
}
