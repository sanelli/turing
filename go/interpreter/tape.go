package interpreter

import "fmt"

type TuringTape struct {
	emtpySymbol       TuringSymbol
	negativePositions []TuringSymbol
	positivePositions []TuringSymbol
	cursorPosition    int32
}

func MakeNewTape(tapeEmptySymbol TuringSymbol) *TuringTape {
	return &TuringTape{
		tapeEmptySymbol,
		make([]TuringSymbol, 0),
		make([]TuringSymbol, 0),
		0,
	}
}

func (tape *TuringTape) MoveTape(move TapeMove) {
	switch move {
	case None:
		break
	case Left:
		tape.cursorPosition--
	case Right:
		tape.cursorPosition++
	default:
		panic(fmt.Sprintf("Unexpected move %d", move))
	}
}

func (tape *TuringTape) Set(symbol TuringSymbol) {
	index := tape.getIndexForCurrentPosition()
	positions := tape.getPositionsListWithEnoughSpace(index)
	positions[index] = symbol
}

func (tape *TuringTape) Get() TuringSymbol {
	index := tape.getIndexForCurrentPosition()
	positions := tape.getPositionsListWithEnoughSpace(index)
	return positions[index]
}

func (tape *TuringTape) Clear() {
	tape.negativePositions = make([]TuringSymbol, 0)
	tape.positivePositions = make([]TuringSymbol, 0)
	tape.cursorPosition = 0
}

func (tape *TuringTape) Initialize(initialSymbols []TuringSymbol, resetCursor bool) {
	tape.Clear()

	for _, symbol := range initialSymbols {
		tape.Set(symbol)
		tape.MoveTape(Right)
	}

	if resetCursor {
		tape.cursorPosition = 0
	}
}

func (tape *TuringTape) InitializeAndResertCursor(initialSymbols []TuringSymbol) {
	tape.Initialize(initialSymbols, true)
}

func (tape *TuringTape) ToString(separator rune) string {
	result := ""

	first := true
	for index := len(tape.negativePositions) - 1; index >= 0; index-- {

		if !first {
			result += string(separator)
		}

		result += string(tape.negativePositions[index])
		first = false
	}

	for index := 0; index < len(tape.positivePositions); index++ {
		if !first {
			result += string(separator)
		}

		result += string(tape.positivePositions[index])
		first = false
	}

	if result != "" {
		result = fmt.Sprintf("%c%s%c", separator, result, separator)
	}

	return result
}

func (tape *TuringTape) getIndexForCurrentPosition() int32 {
	if tape.cursorPosition >= 0 {
		return tape.cursorPosition
	}
	return -tape.cursorPosition - 1
}

func (tape *TuringTape) getPositionsListWithEnoughSpace(index int32) []TuringSymbol {
	if tape.cursorPosition >= 0 {
		for int32(len(tape.positivePositions)) <= index {
			tape.positivePositions = append(tape.positivePositions, tape.emtpySymbol)
		}
		return tape.positivePositions
	}

	for int32(len(tape.negativePositions)) <= index {
		tape.negativePositions = append(tape.negativePositions, tape.emtpySymbol)
	}
	return tape.negativePositions
}
