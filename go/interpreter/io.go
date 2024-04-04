package interpreter

import (
	"fmt"
	"os"
)

func LoadTuringMachinFromFile(format string, filename string) *TuringMachine {
	bytes, error := os.ReadFile(filename)
	if error != nil {
		panic(error.Error())
	}

	return LoadTuringMachine(format, string(bytes))
}

func LoadTuringMachine(format string, representation string) *TuringMachine {
	switch format {
	case "toml":
		return loadMachineFromToml(representation)
	}

	panic(fmt.Sprintf("Unknown format %s", format))
}

func loadMachineFromToml(toml string) *TuringMachine {
	return nil
}
