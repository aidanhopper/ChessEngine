package types

import (
	"database/sql"
	"github.com/gorilla/websocket"
	"sync"
)

type PossibleMove struct {
	StartingSquare  string `json:"startingSquare"`
	TargetSquare    string `json:"targetSquare"`
	IsCapture       bool   `json:"isCapture"`
	IsPawnPromotion bool   `json:"isPawnPromotion"`
}

type Message struct {
	Type string `json:"type"`
	Body []byte `json:"body"`
}

type GameStateMessage struct {
	IsStarted     bool           `json:"isStarted"`
	Fen           string         `json:"fen"`
	PossibleMoves []PossibleMove `json:"possibleMoves"`
	IsMyTurn      bool           `json:"isMyTurn"`
	LastMove      []string       `json:"lastMove"`
	IsCheckMate   bool           `json:"isCheckMate"`
	SoundToPlay   string         `json:"soundToPlay"`
}

type Fen struct {
	PiecePlacement        string
	SideToMove            string
	CastlingAbility       string
	EnPassantTargetSquare string
	HalfmoveClock         string
	FullmoveClock         string
}

type Lobby struct {
	Id            string
	Fen           string
	Sessions      []string
	IsGameStarted bool
	LastMove      []string
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
	Type  string
	Lobby string
	Move  []string
}

type App struct {
	Db          *sql.DB
	Connections map[string]*websocket.Conn
	Mutex       *sync.Mutex
}
