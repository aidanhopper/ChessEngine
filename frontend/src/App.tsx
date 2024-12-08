import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Lobby from './pages/Lobby';
import PageNotFound from './pages/PageNotFound';
import { useNavigate } from 'react-router-dom';

const Navbar = () => {

  const navigate = useNavigate();

  return (
    <>
      <div className="flex-auto z-10 fixed h-16 w-screen bg-black
        content-center backdrop-blur-lg">
        <span className="container font-bold text-xl w-screen m-auto max-h-fit
          flex flex-row items-center content-center text-white">
          <button
            className="w-12 mx-10"
            onClick={() => navigate("/")}>
            <img
              className="rounded-lg invert
            hover:bg-black hover:bg-opacity-20 duration-100 ease-in-out"
              alt="home"
              src="/assets/pb.png" />
          </button>
          <a
            className="flex border-black rounded-lg hover:bg-white hover:bg-opacity-20
            hover:text-white py-1 px-8 italic duration-100 ease-in-out"
            href="https://ahop.dev"
            target="_blank"
            rel="noreferrer noopener">
            portfolio
          </a>
          <a
            className="ml-auto w-12"
            href="https://github.com/aidanhopper/ChessEngine"
            rel="noreferrer"
            target="_blank">
            <img
              className="mr-10 p-1 hover:bg-black hover:bg-opacity-20 rounded-lg
              ease-in-out duration-100 invert"
              src="/assets/github.png"
              alt="github" />
          </a>
        </span>
      </div>
      <div className="bg-white min-h-10 max-h-10" />
    </>
  );
}

const App = () => {
  return (
    <Router>
      <div className=" flex flex-col min-h-screen bg-black">
        <Navbar />
        <div className="flex-1 flex flex-col ">
          <Routes>
            <Route path="/" element={<Home />} />
            <Route path="/:lobby" element={<Lobby />} />
            <Route path="/page-not-found" element={<PageNotFound />} />
          </Routes>
        </div>
        <div className="text-center content-end w-screen">
          <p className="text-gray-700 text-sm mb-1
        ">
            made by @aidanhopper 2024
          </p>
        </div>
      </div>
    </Router>
  );
}

export default App;
