export const playMoveSound = () => {
  new Audio('/sounds/move.mp3')
    .play()
    .catch(error => console.error('Move sound failed: ', error));
}

export const playCaptureSound = () => {
  new Audio('/sounds/capture.mp3')
    .play()
    .catch(error => console.error('Capture sound failed: ', error));
}

