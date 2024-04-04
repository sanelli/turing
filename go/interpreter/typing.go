package interpreter

import "fmt"

type TuringState string
type TuringSymbol rune
type TapeMove byte

// From https://stackoverflow.com/a/70517497/1468832
type Pair[T, U any] struct {
	First  T
	Second U
}

const (
	None TapeMove = iota
	Left
	Right
)

func (move TapeMove) ToString() string {
	switch move {
	case None:
		return "none"
	case Left:
		return "left"
	case Right:
		return "right"
	}
	return "unknown"
}

func ToTapeMove(str string) TapeMove {
	switch str {
	case "none":
		return None
	case "left":
		return Left
	case "right":
		return Right
	}

	panic(fmt.Sprintf("Unknown move '%s'", str))
}
