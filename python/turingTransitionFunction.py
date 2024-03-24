from turingTape import TuringTapeMove
from turingTyping import TuringState, TuringSymbol, raiseIfInvalidState, raiseIfInvalidSymbol

class TuringTransitionFunction():
    _haltState:TuringState
    _transitions:dict[tuple[TuringState, TuringSymbol], tuple[TuringState,TuringSymbol,TuringTapeMove]]

    def __init__(self, haltState:TuringState) -> None:
        raiseIfInvalidState(haltState)
        self._haltState = haltState
        self._transitions = {}
    
    def set(self, frm:tuple[TuringState, TuringSymbol], to:tuple[TuringState,TuringSymbol,TuringTapeMove]) -> None:
        raiseIfInvalidState(frm[0])
        raiseIfInvalidSymbol(frm[1])
        raiseIfInvalidState(to[0])
        raiseIfInvalidSymbol(to[1])

        if frm in self._transitions.keys():
            raise ValueError("From setup has already been defined")
        
        self._transitions[frm] = to
    
    def get(self, state:TuringState, symbol:TuringSymbol) -> tuple[TuringState,TuringSymbol,TuringTapeMove]:
        raiseIfInvalidState(state)
        raiseIfInvalidSymbol(symbol)
    
        frm:tuple[TuringState,TuringSymbol] = (state, symbol)
        if frm not in self._transitions.keys():
            return (self._haltState, symbol, TuringTapeMove.Stay)
        return self._transitions[frm]
        