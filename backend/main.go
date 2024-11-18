package main

import (
	"database/sql"
	"encoding/json"
	"fmt"
	"log"
	"math/rand"
	"net/http"
	"sync"

	"github.com/gorilla/websocket"
	"github.com/lib/pq"
	_ "github.com/lib/pq"
)

const LOBBY_ID_SIZE = 6
const START_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

type Lobby struct {
	Id            string
	Fen           string
	Sessions      []string
	IsGameStarted bool
}

type CreateLobbyMessage struct {
	Ok    bool   `json:"ok"`
	Lobby string `json:"lobby"`
}

type PresentMessage struct {
	Ok   bool   `json:"ok"`
	Info string `json:"info"`
}

type WebSocketReceivingMessage struct {
	Lobby string
}

type PossibleMove struct {
	StartingSquare string `json:"startingSquare"`
	EndingSquare   string `json:"endingSquare"`
	IsCapture      bool   `json:"isEnPassant"`
	IsPawnPromo    bool   `json:"isPawnPromo"`
}

type GameStateMessage struct {
	IsStarted     bool           `json:"isStarted"`
	Fen           string         `json:"fen"`
	PossibleMoves []PossibleMove `json:"possibleMoves"`
}

type App struct {
	DB          *sql.DB
	connections map[string]*websocket.Conn
	mutex       *sync.Mutex
}

func generateRandomString(n int) string {
	const letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
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

func (app *App) createLobbyHandler(w http.ResponseWriter, r *http.Request) {
	middleware(&w)

	lobby := generateRandomString(LOBBY_ID_SIZE)

	response := CreateLobbyMessage{
		Ok:    true,
		Lobby: lobby,
	}

	// Insert the lobby into the database
	_, err := app.DB.Exec(
		"INSERT INTO lobbies (lobby_id, fen, players_present, is_game_started) VALUES ($1, $2, $3, $4)",
		lobby,
		START_FEN,
		pq.Array([]string{}),
		false)

	if err != nil {
		log.Fatalf("Failed to insert empty array: %v", err)
	}

	json.NewEncoder(w).Encode(response)
}

func (app *App) presentHandler(w http.ResponseWriter, r *http.Request) {
	middleware(&w)

	queryParams := r.URL.Query()

	lobby := queryParams.Get("lobby")
	session := queryParams.Get("session")

	if session == "" || lobby == "" {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Need to include session and lobby in the query parameters",
		})
		return
	}

	if len(lobby) != LOBBY_ID_SIZE {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Invalid lobby code",
		})
		return
	}

	if len(session) != 36 {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Invalid lobby code",
		})
		return
	}

	var playersPresent []string
	err := app.DB.QueryRow("SELECT lobbies.players_present FROM lobbies WHERE lobby_id = $1", lobby).Scan(pq.Array(&playersPresent))

	if err != nil {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Lobby does not exist",
		})
		fmt.Println(err)
		return
	}

	for i := range len(playersPresent) {
		if playersPresent[i] == session {
			json.NewEncoder(w).Encode(PresentMessage{
				Ok:   true,
				Info: "Player already in lobby",
			})
			return
		}
	}

	if len(playersPresent) >= 2 {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Lobby is full",
		})
		return
	}

	playersPresent = append(playersPresent, session)

	_, err = app.DB.Exec(
		"UPDATE lobbies SET players_present = $1 WHERE lobby_id = $2",
		pq.Array(playersPresent),
		lobby)

	if err != nil {
		json.NewEncoder(w).Encode(PresentMessage{
			Ok:   false,
			Info: "Updating players in lobby failed",
		})
		fmt.Println(err)
		return
	}

	json.NewEncoder(w).Encode(PresentMessage{
		Ok:   true,
		Info: "Added player to the lobby",
	})
}

func openDb() *sql.DB {
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

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

func (app *App) handleWebSocket(w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println("Failed to upgrade connection:", err)
		return
	}
	defer conn.Close()

	session := r.URL.Query().Get("session")

	app.mutex.Lock()
	app.connections[session] = conn
	app.mutex.Unlock()


	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			log.Println("Error", err)
			break
		}

		var msg WebSocketReceivingMessage
		err = json.Unmarshal(p, &msg)
		if err != nil {
			log.Println("Bad formatting", err)
		} else {
			var lobby Lobby
			err, lobby = app.getLobby(msg.Lobby)

			if err == nil {

				isInLobby := false
				for _, s := range lobby.Sessions {
					if s == session {
						isInLobby = true
					}
				}

				if isInLobby && len(lobby.Sessions) == 2 {
					response := GameStateMessage{
						IsStarted:     true,
						Fen:           lobby.Fen,
						PossibleMoves: []PossibleMove{},
					}

					jsonData, err := json.Marshal(response)
					if err == nil {
						app.broadcastToSessions(lobby.Sessions, jsonData)
					}
				}
			}
		}
	}

	app.mutex.Lock()
	delete(app.connections, session)
	app.mutex.Unlock()
}

func (app *App) broadcastToAllLobbies(message string) {
	app.mutex.Lock()
	for _, conn := range app.connections {
		err := conn.WriteMessage(websocket.TextMessage, []byte(message))
		if err != nil {
			log.Println("Error broadcasting message:", err)
		}
	}
	app.mutex.Unlock()
}

func (app *App) getLobby(lobby string) (error, Lobby) {
	var lobbyId string
	var fen string
	var playersPresent []string
	var isGameStarted bool

	err := app.DB.QueryRow(
		"SELECT lobby_id, fen, players_present, is_game_started FROM lobbies WHERE lobby_id = $1",
		lobby).Scan(&lobbyId, &fen, pq.Array(&playersPresent), &isGameStarted)

	if err != nil {
		log.Println("Lobby", lobby, "not found")
		return err, Lobby{}
	}

	return nil, Lobby{
		lobbyId,
		fen,
		playersPresent,
		isGameStarted,
	}
}

func (app *App) broadcastToSessions(sessions []string, message []byte) {
	app.mutex.Lock()
	for _, session := range sessions {
		err := app.connections[session].WriteMessage(websocket.TextMessage, message)
		if err != nil {
			log.Println("Error broadcasting to lobby:", err)
		}
	}
	app.mutex.Unlock()
}

func handleEndpoints(app *App) {
	http.HandleFunc("/api/v1/create-lobby", app.createLobbyHandler)
	http.HandleFunc("/api/v1/present", app.presentHandler)
	http.HandleFunc("/ws", app.handleWebSocket)
}

func serve() {
	fmt.Println("Starting server on 4000")
	if err := http.ListenAndServe(":4000", nil); err != nil {
		fmt.Println("Server failed to start:", err)
	}
}

func main() {
	db := openDb()
	defer db.Close()
	app := &App{
		DB:          db,
		connections: make(map[string]*websocket.Conn),
		mutex:       &sync.Mutex{},
	}
	handleEndpoints(app)
	serve()
}
