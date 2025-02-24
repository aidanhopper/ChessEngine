import { useNavigate } from 'react-router-dom';
import { createLobby } from '../Query';
import Game from '../Game';

const FrontPageBoard = ({ tileSize, fen }: { tileSize: number, fen: string }) => {
  return (
    <div className="max-w-fit m-auto">
      <Game
        fen={fen}
        onBlackMove={(_start, _end) => { }}
        onWhiteMove={(_start, _end) => { }}
        tileSize={tileSize}
        lastMove={[]}
        disabledSides="wb"
      />
    </div>
  );
}

const PlayButtons = ({ navigate }: { navigate: any }) => {
  return (
    <div className="flex flex-col">
      <div className="flex-auto p-1 font font-extrabold text-4xl text-transparent bg-gradient-to-r
        from-white to-gray-400 inline-block bg-clip-text">
        A open source <br /> online chess site
      </div>
      <button
        onClick={() => {
          createLobby("1v1").then(res => {
            if (res.ok) {
              navigate(`/${res.lobby}`, { state: "creator" })
            }
          })
        }}
        className="flex-auto rounded-lg text-black
        font-bold m-auto px-12 md:w-96 py-2 mt-8 mb-4 hover:invert-[70%] bg-white
        duration-100 ease-in-out">
        Play with a friend
      </button>
      <button
        onClick={() => {
          createLobby("1vcomputer").then(res => {
            if (res.ok) {
              navigate(`/${res.lobby}`, { state: "creator" })
            }
          })
        }}
        className="flex-auto rounded-lg text-black
        font-bold m-auto px-12 md:w-96 py-2 mb-4 hover:invert-[70%] bg-white
        duration-100 ease-in-out">
        Play with the computer
      </button>
      <button
        className="flex-auto rounded-lg text-black
        font-bold m-auto px-12 md:w-96 py-2 mb-4 hover:invert-[70%] bg-white
        duration-100 ease-in-out"
        onClick={() => {
          navigate("/fen-viewer")
        }}>
        Fen Viewer
      </button>
    </div>
  );
}

const Home = () => {

  const navigate = useNavigate();

  return (
    <>
      <div className="flex flex-col container m-auto mt-[7rem] overflow-hidden">
        <div className="flex flex-col md:flex-row m-auto">
          <div className="flex-auto m-auto p-5 text-center content-center">
            <PlayButtons navigate={navigate} />
          </div>
          <div className="flex-auto hidden lg:block m-auto h-fill">
            <FrontPageBoard tileSize={50} fen="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" />
          </div>
        </div>
        <div className="flex-auto text-center content-center h-[10rem] italic text-gray-700">
          <p>
            more to come...
          </p>
        </div>
      </div>
    </>
  );
}

export default Home;
