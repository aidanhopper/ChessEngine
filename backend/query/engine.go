package query

import (
	"backend/types"
	"encoding/json"
	"io"
	"log"
	"net/http"
	"net/url"
	"os"
)

func PossibleMoves(fen string) (error, []types.PossibleMove) {
	engineurl := os.Getenv("ENGINE_URL")
	resp, err :=
		http.Get(engineurl + "/possible-moves?fen=" + url.QueryEscape(fen))
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
	engineurl := os.Getenv("ENGINE_URL")
	resp, err :=
		http.Get(engineurl + "/make-move?fen=" + url.QueryEscape(fen) + "&start=" + start + "&target=" + target)
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

func MoveInfo(fen string, start string, target string) (error, types.PossibleMove) {
	engineurl := os.Getenv("ENGINE_URL")
	resp, err :=
		http.Get(engineurl + "/move-info?fen=" + url.QueryEscape(fen) + "&start=" + start + "&target=" + target)
	if err != nil {
		log.Println("Error:", err)
		return err, types.PossibleMove{}
	}

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Println("Error:", err)
		return err, types.PossibleMove{}
	}

	var move types.PossibleMove
	err = json.Unmarshal(body, &move)
	if err != nil {
		log.Println("Error:", err)
		return err, types.PossibleMove{}
	}

	return nil, move
}
