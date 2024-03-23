from turingTyping import TuringState, TuringSymbol
from turingTape import TuringTape, TuringTapeMove
from turingTransitionFunction import TuringTransitionFunction

class TuringMachine():
    _initialState:TuringState
    _finalStates:set[TuringState]
    _symbols:set[TuringSymbol]
    _tape:TuringTape
    _transitions:TuringTransitionFunction
    _currentState:TuringState

    def __init__(self) -> None:
        pass
