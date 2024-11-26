package main

import (
	"backend/database"
	"backend/handlers"
	"backend/types"
	"encoding/json"
	"fmt"
	"github.com/joho/godotenv"
	"log"
	"math/rand"
	"net/http"
	"os"
	"path/filepath"
	"sync"

	"github.com/gorilla/websocket"

	"github.com/lib/pq"
	_ "github.com/lib/pq"
)

const LOBBY_ID_SIZE = 6
const START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

func generateRandomString(n int) string {
	const letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	b := make([]byte, n)
	for i := range b {
		b[i] = letters[rand.Intn(len(letters))]
	}
	return string(b)
}

func middleware(w *http.ResponseWriter) {
	(*w).Header().Set("Content-Type", "application/json")
	(*w).Header().Set("Access-Control-Allow-Origin", "*")
}

func CreateLobbyHandler(app *types.App, w http.ResponseWriter, r *http.Request) {
	middleware(&w)

	queryParams := r.URL.Query()
	lobby_type := queryParams.Get("type")

	lobby := generateRandomString(LOBBY_ID_SIZE)

	response := types.CreateLobbyMessage{
		Ok:    true,
		Lobby: lobby,
	}

	lastMove, _ := json.Marshal([]string{})

	// Insert the lobby into the database
	_, err := app.Db.Exec(
		"INSERT INTO lobbies (lobby_id, fen, players_present, is_game_started, last_move, lobby_type) VALUES ($1, $2, $3, $4, $5, $6)",
		lobby,
		START_FEN,
		pq.Array([]string{}),
		false,
		lastMove,
		lobby_type)

	if err != nil {
		log.Fatalf("Failed to insert empty array: %v", err)
	}

	json.NewEncoder(w).Encode(response)
}

func PresentHandler(app *types.App, w http.ResponseWriter, r *http.Request) {
	middleware(&w)

	queryParams := r.URL.Query()

	lobby := queryParams.Get("lobby")
	session := queryParams.Get("session")

	if session == "" || lobby == "" {

		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: []byte("Need to include session and lobby in the query parameters"),
		})
		return
	}

	if len(lobby) != LOBBY_ID_SIZE {
		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: []byte("Invalid lobby code"),
		})
		return
	}

	if len(session) != 36 {
		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: []byte("Invalid lobby code"),
		})
		return
	}

	var playersPresent []string
	err := app.Db.QueryRow("SELECT lobbies.players_present FROM lobbies WHERE lobby_id = $1", lobby).Scan(pq.Array(&playersPresent))

	if err != nil {
		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: []byte("Lobby does not exist"),
		})
		log.Println(err)
		return
	}

	_, lobbyData := database.GetLobby(app, lobby)

	type Info struct {
		Type    string `json:"type"`
		Message any    `json:"message"`
	}

	for i := range len(playersPresent) {
		if playersPresent[i] == session {
			data := Info{
				Type:    lobbyData.Type,
				Message: "Player is already in the lobby",
			}

			json.NewEncoder(w).Encode(types.PresentMessage{
				Ok:   true,
				Body: data,
			})
			return
		}
	}

	if len(playersPresent) >= 2 {
		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: "Lobby is full",
		})
		return
	}

	playersPresent = append(playersPresent, session)

	_, err = app.Db.Exec(
		"UPDATE lobbies SET players_present = $1 WHERE lobby_id = $2",
		pq.Array(playersPresent),
		lobby)

	if err != nil {
		json.NewEncoder(w).Encode(types.PresentMessage{
			Ok:   false,
			Body: []byte("Updating players in lobby failed"),
		})
		log.Println(err)
		return
	}

	data := Info{
		Type:    lobbyData.Type,
		Message: "Added player to the lobby",
	}

	json.NewEncoder(w).Encode(types.PresentMessage{
		Ok:   true,
		Body: data,
	})
}

func handleEndpoints(app *types.App) {
	http.HandleFunc("/api/v1/create-lobby", func(w http.ResponseWriter, r *http.Request) {
		CreateLobbyHandler(app, w, r)
	})
	http.HandleFunc("/api/v1/present", func(w http.ResponseWriter, r *http.Request) {
		PresentHandler(app, w, r)
	})
	http.HandleFunc("/api/v1/ws", func(w http.ResponseWriter, r *http.Request) {
		handlers.WebSocketHandler(app, w, r)
	})
}

func serve() {
	fmt.Println("Starting server on 4000")
	if err := http.ListenAndServe(":4000", nil); err != nil {
		fmt.Println("Server failed to start:", err)
	}
}

func main() {
	godotenv.Load()

	db := database.Open()
	defer db.Close()
	app := &types.App{
		Db:          db,
		Connections: make(map[string]*websocket.Conn),
		Mutex:       &sync.Mutex{},
	}
	handleEndpoints(app)

	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		path := filepath.Join("./build", r.URL.Path)
		_, err := os.Stat(path)

		// If the requested file exists, serve it
		if err == nil {
			http.ServeFile(w, r, path)
			return
		}

		// Otherwise, serve index.html for React Router routes
		http.ServeFile(w, r, filepath.Join("./build", "index.html"))
	})

	serve()
}
