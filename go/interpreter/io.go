package interpreter

import (
	"fmt"
	"os"

	"github.com/pelletier/go-toml/v2"
)

func LoadTuringMachinFromFile(format string, filename string) *TuringMachine {
	bytes, error := os.ReadFile(filename)
	if error != nil {
		panic(error.Error())
	}

	return LoadTuringMachine(format, bytes)
}

func LoadTuringMachine(format string, representation []byte) *TuringMachine {
	switch format {
	case "toml":
		return loadMachineFromToml(representation)
	}

	panic(fmt.Sprintf("Unknown format %s", format))
}

func loadMachineFromToml(representation []byte) *TuringMachine {
	var turingToml TuringToml
	err := toml.Unmarshal(representation, &turingToml)
	if err != nil {
		panic("Cannot parse TOML representation")
	}

	states := make([]TuringState, len(turingToml.States))
	for index, state := range turingToml.States {
		states[index] = TuringState(state)
	}

	finalStates := make([]TuringState, len(turingToml.FinalStates))
	for index, state := range turingToml.FinalStates {
		finalStates[index] = TuringState(state)
	}

	symbols := make([]TuringSymbol, len(turingToml.Symbols))
	for index, symbol := range turingToml.Symbols {
		if len(symbol) != 1 {
			panic(fmt.Sprintf("Symbol '%s' must have length 1 but has length %d", symbol, len(symbol)))
		}
		symbols[index] = TuringSymbol(symbol[0])
	}

	if len(turingToml.EmptySymbol) != 1 {
		panic(fmt.Sprintf("Empty symbol '%s' must have length 1 but has length %d", turingToml.EmptySymbol, len(turingToml.EmptySymbol)))
	}

	transitions := make([]Pair[TuringTransitionFunctionFrom, TuringTransitionFunctionTo], len(turingToml.Transitions))
	for index, transition := range turingToml.Transitions {

		if len(transition.Symbol) != 1 {
			panic(fmt.Sprintf("Symbol '%s' must have length 1 but has length %d", transition.Symbol, len(transition.Symbol)))
		}

		if len(transition.NewSymbol) != 1 {
			panic(fmt.Sprintf("Symbol '%s' must have length 1 but has length %d", transition.Symbol, len(transition.NewSymbol)))
		}

		move := ToTapeMove(transition.Move)

		transitions[index] = Pair[TuringTransitionFunctionFrom, TuringTransitionFunctionTo]{
			First:  TuringTransitionFunctionFrom{TuringState(transition.State), TuringSymbol(transition.Symbol[0])},
			Second: TuringTransitionFunctionTo{TuringState(transition.NewState), TuringSymbol(transition.NewSymbol[0]), move},
		}
	}

	return MakeNewTuringMachine(
		states,
		TuringState(turingToml.InitialState),
		finalStates,
		symbols,
		TuringSymbol(turingToml.EmptySymbol[0]),
		transitions)
}

type TuringTomlTransition struct {
	State     string
	Symbol    string
	NewState  string
	NewSymbol string
	Move      string
}

type TuringToml struct {
	States       []string
	InitialState string
	FinalStates  []string
	Symbols      []string
	EmptySymbol  string
	Transitions  []TuringTomlTransition
}
