package main

import (
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

	tape := interpreter.TuringTape{}
	tape.MakeNewTape(' ')

	os.Exit(0)
}
