export const createLobby = async () => {
  const response = await fetch(`/api/v1/create-lobby`, {
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

  return response
}

export const present = async (lobby: string, sessionId: string) => {
  const url = `/api/v1/present?lobby=${lobby}&session=${sessionId}`;
  const response = await fetch(url, {
    method: 'GET',
    mode: 'cors',
  })
    .then(response => response.json());

  return response
}
