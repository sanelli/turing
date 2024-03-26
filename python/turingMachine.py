from turingTyping import TuringState, TuringSymbol, raiseIfInvalidState, raiseIfInvalidSymbol, raiseIfUnknwodSymbol, raiseIfUnknwodState
from turingTape import TuringTape, TuringTapeMove
from turingTransitionFunction import TuringTransitionFunction

import collections.abc

class TuringMachine():
    _initialState:TuringState
    _finalStates:set[TuringState]
    _symbols:set[TuringSymbol]
    _currentState:TuringState
    _tape:TuringTape
    _transitions:TuringTransitionFunction

    def __init__(self,
                states:collections.abc.Iterable[str],
                initialState:str,
                finalStates:collections.abc.Iterable[str],
                symbols:collections.abc.Iterable[str],
                emptySymbol:str,
                transitions:dict[tuple[TuringState,TuringSymbol],tuple[TuringState,TuringSymbol,TuringTapeMove]]) -> None:
        
        if len(states) < 1:
            raise ValueError("At least one state must be provided")

        for state in states:
            raiseIfInvalidState(state)

        raiseIfInvalidState(initialState)
        raiseIfUnknwodState(initialState, states, "Initial state")
        self._initialState = initialState
        self._currentState = initialState

        if len(finalStates) < 1:
            raise ValueError("At least one final state must be provided")

        for state in finalStates:
            raiseIfInvalidState(state, "Final state")
            raiseIfUnknwodState(state, states, "Final state")
        self._finalStates = set(finalStates)

        if len(symbols) < 1:
            raise ValueError("At least one symbol must be provided")

        for symbol in symbols:
            raiseIfInvalidSymbol(symbol)
        self._symbols = set(symbols)

        raiseIfInvalidSymbol(emptySymbol)
        raiseIfUnknwodSymbol(emptySymbol, symbols)
        self._tape = TuringTape(emptySymbol)

        defaultFianlState:str = next(iter(self._finalStates))
        self._transitions = TuringTransitionFunction(defaultFianlState)

        for frm in transitions:
            to = transitions[frm]
            raiseIfInvalidState(frm[0], "From state")
            raiseIfUnknwodState(frm[0], states, "From state")
            raiseIfInvalidSymbol(frm[1], "From state")
            raiseIfUnknwodSymbol(frm[1], symbols, "From symbol")
            raiseIfInvalidState(to[0], "Target state")
            raiseIfUnknwodState(to[0], states, "Target state")
            raiseIfInvalidSymbol(to[1], "Target symbol")
            raiseIfUnknwodSymbol(to[1], symbols, "Target symbol")

            self._transitions.set(frm, to)
    
    def currentState(self) -> TuringState:
        return self._currentState

    def tape(self) -> str:
        return self._tape.toString()
    
    def clear(self, initialTape:collections.abc.Iterable[TuringSymbol]):
        for symbol in initialTape:
            raiseIfInvalidSymbol(symbol)
            raiseIfUnknwodSymbol(symbol, self._symbols)

        self._currentState = self._initialState
        self._tape.initialize(initialTape)

    def halted(self) -> bool:
        return self._currentState in self._finalStates
    
    def step(self):

        if not self.halted():
            transition = self._transitions.get(self._currentState, self._tape.getSymbol())
            self._currentState = transition[0]
            self._tape.setSymbol(transition[1])
            self._tape.move(transition[2])

    def run(self):
        while not self.halted():
            self.step()
