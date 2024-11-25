import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Lobby from './pages/Lobby';
import PageNotFound from './pages/PageNotFound';

const App = () => {

  return (
    <div className=" flex flex-col min-h-screen bg-white">
      <div className="flex-auto z-10 fixed h-16 w-screen bg-gradient-to-r from-cyan-500/80 to-purple-500/80
        content-center backdrop-blur-lg">
        <span className="container font-bold text-xl w-screen m-auto max-h-fit
          flex flex-row items-center content-center text-white">
          <a href="/">
            <img
              className="w-10 border-2 border-black rounded-lg invert
            hover:bg-black hover:bg-opacity-20 duration-100 ease-in-out mx-10"
              alt="home"
              src="/assets/pb.png" />
          </a>
          <a className="flex border-black rounded-lg hover:bg-white hover:bg-opacity-20
            hover:text-white py-1 px-8 italic" href="/">
            portfolio
          </a>
          <a
            className="flex ml-auto"
            href="https://github.com/aidanhopper/ChessEngine"
            rel="noreferrer"
            target="_blank">
            <img
              className="w-10 mr-10 p-1 hover:bg-black hover:bg-opacity-20 rounded-lg
              border-2 border-black ease-in-out duration-100 invert"
              src="/assets/github.png"
              alt="github" />
          </a>
        </span>
      </div>
      <div className="bg-white min-h-16 max-h-16" />
      <div className="flex-1 flex flex-col ">
        <Router>
          <Routes>
            <Route path="/" element={<Home />} />
            <Route path="/:lobby" element={<Lobby />} />
            <Route path="/page-not-found" element={<PageNotFound />} />
          </Routes>
        </Router>
      </div>
      <div className="text-center content-end w-screen">
        <div className="bg-gray-100 text-white font-extrabold italic
        bg-gradient-to-r from-cyan-400/60 to-purple-400/60">
          made by @aidanhopper 2024
        </div>
      </div>
    </div>
  );
}

export default App;
