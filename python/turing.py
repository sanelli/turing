#!/usr/bin/python3

from turingMachine import TuringMachine

import sys

def main(arv:list[str]) -> None:
    machine:TuringMachine = TuringMachine()
    return 0

if __name__ == "__main__":
    returnCode = main(sys.argv[1::])
    exit(returnCode)