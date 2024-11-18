import { BrowserRouter as Router, Routes, Route } from 'react-router-dom';
import Home from './pages/Home';
import Lobby from './pages/Lobby';
import PageNotFound from './pages/PageNotFound';

const App = () => {
  return (
    <div className="h-screen w-screen bg-white">
      <Router>
        <Routes>
          <Route path="/" element={<Home />} />
          <Route path="/:lobby" element={<Lobby />} />
          <Route path="/page-not-found" element={<PageNotFound />} />
        </Routes>
      </Router>
    </div>
  );
}

export default App;
