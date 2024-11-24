//const BACKEND_URL = process.env.BACKEND_URL;
const BACKEND_URL = "http://localhost:4000"

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
