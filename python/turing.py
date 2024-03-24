#!/usr/bin/python3

from turingMachineIO import turingMachineFromFormatString
from pathlib import Path

import sys

def main(argv:list[str]) -> int:
    if len(argv) < 3:
        print(f"Usage: python3 ./turing.py <format> <filepath> <initialTape>")
        return 1
    
    machine = turingMachineFromFormatString(argv[0], Path(argv[1]).read_text())
    machine.clear([symbol for symbol in argv[2]])

    print(f"Initial tape: {machine.tape()}")
    machine.run()

    print(f"Final tape: {machine.tape()}")
    print(f"Final state: {machine.currentState()}")
    return 0 if machine.halted() else 1

if __name__ == "__main__":
    returnCode = main(sys.argv[1::])
    exit(returnCode)