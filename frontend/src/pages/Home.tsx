import { useNavigate } from 'react-router-dom';
import { createLobby } from '../Query';

const Home = () => {

  const navigate = useNavigate();

  return (
    <div className="flex h-screen w-screen overflow-hidden">
      <div className="m-auto">
        <button
          onClick={() => {
            createLobby().then(res => {
              if (res.ok) {
                navigate(`/${res.lobby}`, { state: "creator" })
              }
            });
          }}
          className="border rounded border-black p-2 hover:bg-black
          hover:text-white transition-colors duration-100 ease-in-out">
          Play with a friend!
        </button>
      </div>
    </div>
  );
}

export default Home;
