package interpreter

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
		return "right"
	case Right:
		return "right"
	}
	return "unknown"
}
