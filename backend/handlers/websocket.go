package handlers

import (
	"backend/database"
	"backend/query"
	"backend/types"
	"encoding/json"
	"log"
	"net/http"
	"strings"

	"github.com/gorilla/websocket"
)

var upgrader = websocket.Upgrader{
	CheckOrigin: func(r *http.Request) bool {
		return true
	},
}

func parseFen(fen string) types.Fen {
	strsplit := strings.Split(fen, " ")
	return types.Fen{
		PiecePlacement:        strsplit[0],
		SideToMove:            strsplit[1],
		CastlingAbility:       strsplit[2],
		EnPassantTargetSquare: strsplit[3],
		HalfmoveClock:         strsplit[4],
		FullmoveClock:         strsplit[5],
	}
}

func broadcastToAllLobbies(app *types.App, message string) {
	app.Mutex.Lock()
	for _, conn := range app.Connections {
		if conn != nil {
			err := conn.WriteMessage(websocket.TextMessage, []byte(message))
			if err != nil {
				log.Println("Error broadcasting message:", err)
			}
		}
		log.Println("This websocket does not exist")
	}
	app.Mutex.Unlock()
}

func broadcastToSessions(app *types.App, sessions []string, message []byte) {
	for _, session := range sessions {
		sendToSession(app, session, message)
	}
}

func sendToSession(app *types.App, session string, message []byte) {
	app.Mutex.Lock()
	if app.Connections[session] != nil {
		err := app.Connections[session].WriteMessage(websocket.TextMessage, message)
		if err != nil {
			log.Println("Failed to send message to session " + session)
		}
	} else {
		log.Println("Socket was closed")
	}
	app.Mutex.Unlock()
}

func isIn(session string, sessions []string) bool {
	isInArr := false
	for _, s := range sessions {
		if s == session {
			isInArr = true
		}
	}
	return isInArr
}

func handleRegisterMessage(
	app *types.App,
	msg types.WebSocketReceivingMessage,
	session string) {
	err, lobby := database.GetLobby(app, msg.Lobby)

	if err == nil {
		isReadyToStart := isIn(session, lobby.Sessions) && len(lobby.Sessions) == 2
		if isReadyToStart {
			_, possibleMoves := query.PossibleMoves(lobby.Fen)
			fenObj := parseFen(lobby.Fen)

			database.UpdateIsGameStarted(app, lobby.Id, true)

			whiteResponse := types.GameStateMessage{
				IsStarted:     true,
				Fen:           lobby.Fen,
				PossibleMoves: possibleMoves,
				IsMyTurn:      fenObj.SideToMove == "w",
				LastMove:      lobby.LastMove,
				IsCheckMate:   len(possibleMoves) == 0,
			}

			blackResponse := types.GameStateMessage{
				IsStarted:     true,
				Fen:           lobby.Fen,
				PossibleMoves: possibleMoves,
				IsMyTurn:      fenObj.SideToMove == "b",
				LastMove:      lobby.LastMove,
				IsCheckMate:   len(possibleMoves) == 0,
        SoundToPlay:   "",
			}

			whiteData, _ := json.Marshal(whiteResponse)
			sendToSession(app, lobby.Sessions[0], whiteData)

			blackData, _ := json.Marshal(blackResponse)
			sendToSession(app, lobby.Sessions[1], blackData)
		}

	}

}

func handleMakeMoveMessage(app *types.App, msg types.WebSocketReceivingMessage, session string) {
	err, lobby := database.GetLobby(app, msg.Lobby)

	// makes sure the lobby exists
	if err == nil {

		// makes sure the session is in the lobby and the game is started
		if isIn(session, lobby.Sessions) && lobby.IsGameStarted {

			currentFenObj := parseFen(lobby.Fen)

			// get the updated fen string from the move
			updatedFens := []string{lobby.Fen}
			if currentFenObj.SideToMove == "w" && session == lobby.Sessions[0] {
				_, updatedFens = query.MakeMove(lobby.Fen, msg.Move[0], msg.Move[1])
			}
			if currentFenObj.SideToMove == "b" && session == lobby.Sessions[1] {
				_, updatedFens = query.MakeMove(lobby.Fen, msg.Move[0], msg.Move[1])
			}

			// if its different then send a response
			if updatedFens[0] != lobby.Fen {
				_, possibleMoves := query.PossibleMoves(updatedFens[0])

				if updatedFens[0] != lobby.Fen {
					database.UpdateFen(app, lobby.Id, updatedFens[0])
					database.UpdateLastMove(app, lobby.Id, msg.Move)
				}

				var updatedFenObjs []types.Fen
				for _, fen := range updatedFens {
					updatedFenObjs = append(updatedFenObjs, parseFen(fen))
				}

				_, moveInfo := query.MoveInfo(lobby.Fen, msg.Move[0], msg.Move[1])
				soundToPlay := "move"
				if moveInfo.IsCapture {
					soundToPlay = "capture"
				}

				whiteResponse := types.GameStateMessage{
					IsStarted:     true,
					Fen:           updatedFens[0],
					PossibleMoves: possibleMoves,
					IsMyTurn:      updatedFenObjs[0].SideToMove == "w",
					LastMove:      msg.Move,
					IsCheckMate:   len(possibleMoves) == 0,
					SoundToPlay:   soundToPlay,
				}

				blackResponse := types.GameStateMessage{
					IsStarted:     true,
					Fen:           updatedFens[0],
					PossibleMoves: possibleMoves,
					IsMyTurn:      updatedFenObjs[0].SideToMove == "b",
					LastMove:      msg.Move,
					IsCheckMate:   len(possibleMoves) == 0,
					SoundToPlay:   soundToPlay,
				}

				whiteData, _ := json.Marshal(whiteResponse)
				sendToSession(app, lobby.Sessions[0], whiteData)

				blackData, _ := json.Marshal(blackResponse)
				sendToSession(app, lobby.Sessions[1], blackData)
			}
		}
	}

}

func WebSocketHandler(app *types.App, w http.ResponseWriter, r *http.Request) {
	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		log.Println("Failed to upgrade connection:", err)
		return
	}
	defer conn.Close()

	session := r.URL.Query().Get("session")

	app.Mutex.Lock()
	app.Connections[session] = conn
	app.Mutex.Unlock()

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			log.Println("Error", err)
			break
		}

		var msg types.WebSocketReceivingMessage
		err = json.Unmarshal(p, &msg)
		if err != nil {
			log.Println("Bad formatting", err)
		} else {
			switch msg.Type {
			case "register":
				handleRegisterMessage(app, msg, session)
				break
			case "makemove":
				handleMakeMoveMessage(app, msg, session)
				break
			}

		}
	}

	app.Mutex.Lock()
	delete(app.Connections, session)
	app.Mutex.Unlock()
}
