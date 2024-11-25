import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Lobby from './pages/Lobby';
import PageNotFound from './pages/PageNotFound';

const App = () => {

  return (
    <div className="bg-white flex flex-col h-screen">
      <div className="flex-auto z-10 sticky top-0 max-h-20 min-h-20 
        content-center bg-white bg-opacity-75 backdrop-blur-lg">
        <span className="container font-bold text-xl w-screen m-auto max-h-fit
          flex flex-row items-center content-center">
          <a href="/">
            <img
              className="w-12 border-4 border-black rounded-lg hover:invert
            hover:bg-white hover:border-white duration-100 ease-in-out mx-10"
              alt="home"
              src="/assets/rb.png" />
          </a>
          <a className="flex border-black rounded-lg hover:bg-black
            hover:text-white py-1 px-8" href="/">
            portfolio
          </a>
          <a
            className="flex ml-auto"
            href="https://github.com/aidanhopper/ChessEngine"
            rel="noreferrer"
            target="_blank">
            <img
              className="w-12 mr-10 p-1 hover:bg-white hover:invert rounded-lg
              border-[4px] border-black hover:border-white ease-in-out duration-100"
              src="/assets/github.png"
              alt="github" />
          </a>
        </span>
      </div>
      <div className="flex-1 flex bg-gray-50">
        <Router>
          <Routes>
            <Route path="/" element={<Home />} />
            <Route path="/:lobby" element={<Lobby />} />
            <Route path="/page-not-found" element={<PageNotFound />} />
          </Routes>
        </Router>
      </div>
    </div>
  );
}

export default App;
