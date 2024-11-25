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
      <div className="flex-auto p-1 font font-extrabold text-4xl">
        The worlds best open source <br /> online chess site
      </div>
      <button
        onClick={onClick}
        className="flex-auto border-2 rounded-lg border-black m-auto px-12 md:w-96 py-2 mt-8 mb-4 hover:bg-black
          hover:text-white transition-colors duration-100 ease-in-out disabled:bg-gray-100">
        Play with a friend!
      </button>
      <button
        onClick={onClick}
        className="flex-auto border-2 rounded-lg border-black m-auto px-12 md:w-96 py-2 hover:bg-black
          hover:text-white transition-colors duration-100 ease-in-out disabled:bg-gray-100 disabled disable:text-gray-400">
        Create a tournament
      </button>
    </div>
  );
}

const Home = () => {

  const navigate = useNavigate();

  return (
    <>
      <div className="flex flex-col container mx-auto mt-8 overflow-hidden">
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
          <div className="flex-auto hidden lg:block mb-20">
            <FrontPageBoard tileSize={50} fen="rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1" />
          </div>
        </div>
        <div className="flex flex-auto flex-col bg-gray-100 min-w-[60%] mx-auto text-center">
          <h1 className="text-4xl font-extrabold w-full p-4">
            Play Tournaments
          </h1>
          <div className="flex flex-col lg:flex-row flex-auto m-auto w-full">
            <div className="flex flex-auto m-auto p-4">
              <FrontPageBoard tileSize={25} fen="8/pK4kP/6p1/2NP2P1/5R2/3p3b/n2BB1Pb/8 w - - 0 1" />
            </div>
            <div className="flex flex-auto m-auto p-4">
              <FrontPageBoard tileSize={25} fen="6K1/1ppB1n2/1P3p2/P3pr1P/3kP1Pp/8/7R/8 w - - 0 1" />
            </div>
            <div className="flex flex-auto m-auto p-4">
              <FrontPageBoard tileSize={25} fen="1Q6/5p1r/1p3p1P/5K2/1Pp1P2p/k6P/7b/2Nq4 w - - 0 1" />
            </div>
            <div className="flex flex-auto m-auto p-4">
              <FrontPageBoard tileSize={25} fen="3Q1N2/pR2P3/K3PP2/nBr1k3/P7/7b/p7/2N5 w - - 0 1" />
            </div>
          </div>
          <div className="flex flex-auto bg-gray-50 w-full h-20" />
          <div className="flex flex-auto flow-col lg:flex-row bg-gray-100 p-4 min-w-[60%] mx-auto text-center">
            <div className="flex-auto">
              <h1 className="text-4xl font-extrabold w-full mb-4">
                Or 1v1
              </h1>
              <div className="flex flex-col lg:flex-row flex-auto m-auto w-full">
                <div className="flex flex-auto m-auto p-2">
                  <FrontPageBoard tileSize={25} fen="8/pK4kP/6p1/2NP2P1/5R2/3p3b/n2BB1Pb/8 w - - 0 1" />
                </div>
              </div>
            </div>
          </div>
          <div className="flex flex-auto bg-gray-50 w-full h-20" />
          <div className="flex flex-row flex-auto text-center w-full p-2 content-center m-auto pl-10">
            <div className="flex-auto m-auto p-4">
              <img className="w-32 float-right rounded-lg border-4 border-black
                shadow shadow-black" src="/assets/magnus.jpg" alt="magnus" />
            </div>
            <div className="flex-auto m-auto text-left">
              <h1 className=" text-2xl font-bold w-fill h-fill">
                This site is better than chess.com!
              </h1>
              <p className="italic">
                - GM Magnus Carlsen <span className="text-sm">(allegedly)</span>
              </p>
            </div>
          </div>
        </div>
      </div>
      <div className="text-center flex-auto content-end w-screen mt-10">
        <div className="bg-gray-100 text-gray-500 font-extrabold italic
        bg-gradient-to-r from-cyan-400/60 to-purple-400/60">
          made by @aidanhopper 2024
        </div>
      </div>
    </>
  );
}

export default Home;
