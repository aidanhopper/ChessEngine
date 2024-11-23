package query

import (
	"backend/types"
	"encoding/json"
	"io"
	"log"
	"net/http"
	"net/url"
)

const CHESS_ENGINE_URL = "http://localhost:6000"

func PossibleMoves(fen string) (error, []types.PossibleMove) {
	resp, err :=
		http.Get(CHESS_ENGINE_URL + "/possible-moves?fen=" + url.QueryEscape(fen))
	if err != nil {
		log.Println("Error:", err)
		return err, nil
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Println("Error:", err)
		return err, nil
	}

	var moves []types.PossibleMove
	err = json.Unmarshal(body, &moves)
	if err != nil {
		log.Println("Error:", err)
		return err, nil
	}

	return nil, moves
}

func MakeMove(fen string, start string, target string) (error, []string) {
	resp, err :=
		http.Get(CHESS_ENGINE_URL + "/make-move?fen=" + url.QueryEscape(fen) + "&start=" + start + "&target=" + target)
	if err != nil {
		log.Println("Make move failed")
		return err, []string{fen}
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Println("Error:", err)
		return err, []string{fen}
	}

	var newfens []string
	err = json.Unmarshal(body, &newfens)
	if err != nil {
		log.Println("Error:", err)
		return err, []string{fen}
	}

	return nil, newfens
}
