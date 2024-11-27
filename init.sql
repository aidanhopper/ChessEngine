CREATE TABLE lobbies (
  lobby_id        varchar(6),
  fen             varchar(150),
  players_present varchar(37)[],
  is_game_started boolean,
  last_move       json,
  lobby_type      varchar(50),
);
