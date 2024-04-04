package main

import (
	"fmt"
	"log"
	"os"
	"turing/interpreter"
)

func main() {
	if len(os.Args) < 4 {
		log.Fatalf("Usage: %s <format> <filename> <tape>", os.Args[0])
		log.Fatal("Available formats:")
		log.Fatal("   - toml: TOML file format")

		os.Exit(1)
	}

	initialTape := make([]interpreter.TuringSymbol, len(os.Args[3]))
	for index, symbol := range os.Args[3] {
		initialTape[index] = interpreter.TuringSymbol(symbol)
	}

	turingMachine := interpreter.LoadTuringMachinFromFile(os.Args[1], os.Args[2])
	//turingMachine.Print()
	turingMachine.Clear(initialTape)

	fmt.Printf("Initial tape: %s\n", turingMachine.GetTape())
	turingMachine.Run()
	fmt.Printf("Final tape: %s\n", turingMachine.GetTape())
	fmt.Printf("Final state: %s\n", turingMachine.GetCurrentState())

	os.Exit(0)
}
