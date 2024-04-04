package interpreter

import (
	"log"
	"testing"
)

func TestSubstitute(test *testing.T) {
	substitute := `
	# Replace all a characters with b and vice-versa.
	# Replace a with b and with a.

	States = [ "replace", "halt" ]
	InitialState = "replace"
	FinalStates = [ "halt" ]
	Symbols = [ " ", "a", "b" ]
	EmptySymbol = " "

	[[Transitions]]
	State = "replace"
	Symbol = "a"
	NewState = "replace"
	NewSymbol = "b"
	Move = "right"

	[[Transitions]]
	State = "replace"
	Symbol = "b"
	NewState = "replace"
	NewSymbol = "a"
	Move = "right"

	[[Transitions]]
	State = "replace"
	Symbol = " "
	NewState = "halt"
	NewSymbol = " "
	Move = "right"
	`
	machine := LoadTuringMachine("toml", []byte(substitute))
	machine.Clear([]TuringSymbol{'a', 'b', 'b', 'a'})
	machine.Run()

	if machine.currentState != "halt" {
		log.Fatalf("current state is supposed to be 'halt' but is '%s'", machine.currentState)
		test.Fail()
	}

	tape := machine.GetTape()
	if tape != "|b|a|a|b| |" {
		log.Fatalf("Tape is supposed to be |b|a|a|b| | but is '%s'", tape)
		test.Fail()
	}
}
