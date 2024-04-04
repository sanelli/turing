package interpreter

import "fmt"

type TuringTransitionFunctionFrom struct {
	State  TuringState
	Symbol TuringSymbol
}

type TuringTransitionFunctionTo struct {
	State  TuringState
	Symbol TuringSymbol
	Move   TapeMove
}

type TuringTransitionFunction struct {
	haltState   TuringState
	transitions map[TuringTransitionFunctionFrom]TuringTransitionFunctionTo
}

func MakeNewTransition(defaultHaltState TuringState) *TuringTransitionFunction {
	return &TuringTransitionFunction{
		defaultHaltState,
		map[TuringTransitionFunctionFrom]TuringTransitionFunctionTo{},
	}
}

func (tape *TuringTransitionFunction) Set(from TuringTransitionFunctionFrom, to TuringTransitionFunctionTo) {
	transition, hasTranistion := tape.transitions[from]
	if hasTranistion {
		panic(fmt.Sprintf("Transition starting from (%s, %c) already exists to (%s, %c, %s)", from.State, from.Symbol, transition.State, transition.Symbol, transition.Move.ToString()))
	}

	tape.transitions[from] = to
}

func (tape *TuringTransitionFunction) Get(from TuringTransitionFunctionFrom) TuringTransitionFunctionTo {
	transition, hasTranistion := tape.transitions[from]

	if !hasTranistion {
		return TuringTransitionFunctionTo{
			tape.haltState,
			from.Symbol,
			None,
		}
	}

	return transition
}

func (tape *TuringTransitionFunction) Print() {
	for from, to := range tape.transitions {
		fmt.Printf("   - (%s, %c) -> (%s, %c, %s)\n", from.State, from.Symbol, to.State, to.Symbol, to.Move.ToString())
	}
}
