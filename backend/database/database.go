package database

import (
	"backend/types"
	"database/sql"
	"encoding/json"
	"log"

	"github.com/lib/pq"
	_ "github.com/lib/pq"
)

func Open() *sql.DB {
	connStr := "postgres://root:root@localhost:5432/postgres?sslmode=disable"
	db, err := sql.Open("postgres", connStr)
	if err != nil {
		log.Fatal(err)
	}

	err = db.Ping()
	if err != nil {
		log.Fatal("Cannot connect to database:", err)
	}

	return db
}

func UpdateFen(app *types.App, lobby string, fen string) {
	_, err := app.Db.Exec(
		"UPDATE lobbies SET fen = $1 WHERE lobby_id = $2",
		fen,
		lobby)
	if err != nil {
		log.Println("There was an error updating the fen string in the db:", err)
	}
}

func GetLobby(app *types.App, lobby string) (error, types.Lobby) {
	var lobbyId string
	var fen string
	var playersPresent []string
	var isGameStarted bool
	var lastMoveJson string

	err := app.Db.QueryRow(
		"SELECT lobby_id, fen, players_present, is_game_started, last_move FROM lobbies WHERE lobby_id = $1",
		lobby).Scan(&lobbyId, &fen, pq.Array(&playersPresent), &isGameStarted, &lastMoveJson)

	var lastMove []string
	json.Unmarshal([]byte(lastMoveJson), &lastMove)

	if err != nil {
		log.Println("Lobby", lobby, "not found")
		return err, types.Lobby{}
	}

	return nil, types.Lobby{
		Id:            lobbyId,
		Fen:           fen,
		Sessions:      playersPresent,
		IsGameStarted: isGameStarted,
		LastMove:      lastMove,
	}
}

func UpdateIsGameStarted(app *types.App, lobby string, isGameStarted bool) {
	_, err := app.Db.Exec(
		"UPDATE lobbies SET is_game_started = $1 WHERE lobby_id = $2",
		isGameStarted,
		lobby)
	if err != nil {
		log.Println("There was an error updating the is_game_started bool in the db:", err)
	}
}

func UpdateLastMove(app *types.App, lobby string, lastMove []string) {
	lastMoveJson, _ := json.Marshal(lastMove)

	_, err := app.Db.Exec(
		"UPDATE lobbies SET last_move = $1 WHERE lobby_id = $2",
		lastMoveJson,
		lobby)
	if err != nil {
		log.Println("There was an error updating the last_move json in the db:", err)
	}
}
