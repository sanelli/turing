package interpreter

type TapeMove byte

const (
	None TapeMove = iota
	Left
	Right
)

type TuringTape struct {
	emtpySymbol       rune
	negativePositions []rune
	positivePositions []rune
	cursorPosition    int32
}

func (tape *TuringTape) MakeNewTape(emptySymbol rune) {
	tape.Clear()
	tape.cursorPosition = 0
}

func (tape *TuringTape) MoveTape(move TapeMove) {
	switch move {
	case None:
		break
	case Left:
		tape.cursorPosition--
	case Right:
		tape.cursorPosition++
	}
}

func (tape *TuringTape) Set(symbol rune) {
	index := tape.getIndexForCurrentPosition()
	positions := tape.getPositionsListWithEnoughSpace(index)
	positions[index] = symbol
}

func (tape *TuringTape) Get() rune {
	index := tape.getIndexForCurrentPosition()
	positions := tape.getPositionsListWithEnoughSpace(index)
	return positions[index]
}

func (tape *TuringTape) Clear() {
	tape.negativePositions = make([]rune, 0)
	tape.positivePositions = make([]rune, 0)
	tape.cursorPosition = 0
}

func (tape *TuringTape) Initialize(initialSymbols []rune, resetCursor bool) {
	tape.Clear()

	for _, symbol := range initialSymbols {
		tape.Set(symbol)
		tape.MoveTape(Right)
	}

	if resetCursor {
		tape.cursorPosition = 0
	}
}

func (tape *TuringTape) InitializeAndResertCursor(initialSymbols []rune) {
	tape.Initialize(initialSymbols, true)
}

func (tape *TuringTape) getIndexForCurrentPosition() int32 {
	if tape.cursorPosition >= 0 {
		return tape.cursorPosition
	}
	return -tape.cursorPosition - 1
}

func (tape *TuringTape) getPositionsListWithEnoughSpace(index int32) []rune {
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
