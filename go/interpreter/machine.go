package interpreter

import (
	"fmt"
	"slices"
)

type TuringMachine struct {
	initialState TuringState
	finalStates  []TuringState
	symbols      []TuringSymbol
	currentState TuringState
	tape         *TuringTape
	transitions  *TuringTransitionFunction
}

func MakeNewTuringMachine(
	states []TuringState,
	initialState TuringState,
	finalStates []TuringState,
	symbols []TuringSymbol,
	emptySymbol TuringSymbol,
	transitions []Pair[TuringTransitionFunctionFrom, TuringTransitionFunctionTo]) *TuringMachine {

	if len(states) <= 0 {
		panic("At least one state must be provided")
	}

	if !slices.Contains(states, initialState) {
		panic(fmt.Sprintf("Initial state %s does not belong to the list of valid states", initialState))
	}

	if len(finalStates) <= 0 {
		panic("At least one final state must be provided")
	}

	for _, finalState := range finalStates {
		if !slices.Contains(states, finalState) {
			panic(fmt.Sprintf("Initial state %s does not belong to the list of valid states", finalState))
		}
	}

	if len(symbols) <= 0 {
		panic("At least one final symbol must be provided")
	}

	if !slices.Contains(symbols, emptySymbol) {
		panic(fmt.Sprintf("Empty symbol %s does not belong to the list of valid symbols", initialState))
	}

	var turingTransitions *TuringTransitionFunction = MakeNewTransition(finalStates[0])
	for _, transition := range transitions {
		if !slices.Contains(states, transition.First.State) {
			panic(fmt.Sprintf("State %s does not belong to the list of valid states", transition.First.State))
		}

		if !slices.Contains(symbols, transition.First.Symbol) {
			panic(fmt.Sprintf("Symbol %c does not belong to the list of valid symbols", transition.First.Symbol))
		}

		if !slices.Contains(states, transition.Second.State) {
			panic(fmt.Sprintf("State %s does not belong to the list of valid states", transition.First.State))
		}

		if !slices.Contains(symbols, transition.Second.Symbol) {
			panic(fmt.Sprintf("Symbol %c does not belong to the list of valid symbols", transition.First.Symbol))
		}

		turingTransitions.Set(transition.First, transition.Second)
	}

	return &TuringMachine{
		initialState: initialState,
		finalStates:  finalStates,
		symbols:      symbols,
		currentState: initialState,
		tape:         MakeNewTape(emptySymbol),
		transitions:  turingTransitions,
	}
}

func (machine *TuringMachine) GetCurrentState() TuringState {
	return machine.currentState
}

func (machine *TuringMachine) GetTape() string {
	return machine.tape.ToString('|')
}

func (machine *TuringMachine) Clear(initialSymbols []TuringSymbol) {

	for _, symbol := range initialSymbols {
		if !slices.Contains(machine.symbols, symbol) {
			panic(fmt.Sprintf("Unknown symbol '%c'", symbol))
		}
	}

	machine.currentState = machine.initialState
	machine.tape.Clear()
	machine.tape.InitializeAndResertCursor(initialSymbols)
}

func (machine *TuringMachine) Halted() bool {
	return slices.Contains(machine.finalStates, machine.currentState)
}

func (machine *TuringMachine) Step() {
	if !machine.Halted() {
		to := machine.transitions.Get(TuringTransitionFunctionFrom{machine.currentState, machine.tape.Get()})
		machine.currentState = to.State
		machine.tape.Set(to.Symbol)
		machine.tape.MoveTape(to.Move)
	}
}

func (machine *TuringMachine) Run() {
	for !machine.Halted() {
		machine.Step()
	}
}

func (machine *TuringMachine) Print() {
	fmt.Printf("Initial state: %s\n", machine.currentState)
	fmt.Printf("Final states: [%s]\n", StatesToString(machine.finalStates))
	fmt.Printf("Symbols: [%s]\n", SymbolsToString(machine.symbols))
	fmt.Printf("Current state: %s\n", machine.currentState)
	fmt.Printf("Tape: |%s|\n", machine.GetTape())
	fmt.Print("Transitions\n")
	machine.transitions.Print()
}
