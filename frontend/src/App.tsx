import Game from './Game'

const App = () => {

  const tileSize = 100;

  return (
    <div className="flex h-screen w-screen overflow-hidden">
      <Game
        className="flex m-auto"
        tileSize={tileSize} />
    </div>
  )

}

export default App;
