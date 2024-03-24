import unittest

from turingMachineIO import turingMachineFromFormatString

class TuringMachineTests(unittest.TestCase):

    def test_substitute(self) -> None:
        description = '''States = [ "replace", "halt" ]
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
'''
        machine = turingMachineFromFormatString("toml", description)
        machine.clear([symbol for symbol in "abba"])
        machine.run()
        self.assertEqual("halt", machine.currentState())
        self.assertEqual("|b|a|a|b|", machine.tape())

if __name__ == '__main__':
    unittest.main()