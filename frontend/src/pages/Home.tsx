import { useNavigate } from 'react-router-dom';
import { createLobby } from '../Query';
import Game from '../Game';

const FrontPageBoard = ({ tileSize, fen }: { tileSize: number, fen: string }) => {
  return (
    <div className="max-w-fit border-8 border-black rounded-lg m-auto">
      <Game
        fen={fen}
        onBlackMove={(start, end) => { }}
        onWhiteMove={(start, end) => { }}
        tileSize={tileSize}
        lastMove={[]}
        disabledSides=""
      />
    </div>
  );
}

const PlayButtons = ({ onClick }: { onClick: () => void }) => {
  return (
    <div className="flex flex-col">
      <div className="flex-auto p-1 font font-extrabold text-4xl text-transparent bg-gradient-to-r from-cyan-600 to-purple-600 inline-block bg-clip-text">
        The worlds best open source <br /> online chess site
      </div>
      <button
        onClick={onClick}
        className="flex-auto border-4 rounded-lg border-gray-400 text-gray-500 font-bold m-auto px-12 md:w-96 py-2 mt-8 mb-4 hover:bg-black
          hover:text-white hover:border-white transition-colors duration-100 ease-in-out">
        Play with a friend!
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
            <PlayButtons onClick={() => {
              createLobby().then(res => {
                if (res.ok) {
                  navigate(`/${res.lobby}`, { state: "creator" })
                }
              });
            }} />
          </div>
          <div className="flex-auto hidden lg:block m-auto h-fill">
            <FrontPageBoard tileSize={50} fen="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" />
          </div>
        </div>
        <div className="flex-auto text-center content-center h-[10rem] italic font-extrabold text-gray-500">
          <p>
            more to come...
          </p>
        </div>
      </div>
    </>
  );
}

export default Home;
