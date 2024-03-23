from turingTape import TuringTapeMove
from turingTyping import TuringState, TuringSymbol

class TuringTransitionFunction():
    _haltState:TuringState
    _transitions:dict[tuple[TuringState, TuringSymbol], tuple[TuringState,TuringSymbol,TuringTapeMove]]

    def __init__(self, haltState) -> None:
        self._haltState = haltState
        self._transitions = {}
    
    def set(self, frm:tuple[TuringState, TuringSymbol], to:tuple[TuringState,TuringSymbol,TuringTapeMove]) -> None:
        if frm in self._transitions.keys():
            raise ValueError("From setup has already been defined")
        
        self._transitions[frm] = to
    
    def get(self, state:TuringState, symbol:TuringSymbol) -> tuple[TuringState,TuringSymbol,TuringTapeMove]:
        frm:tuple[TuringState,TuringSymbol] = (state, symbol)
        if frm not in self._transitions.keys():
            return (self._haltState, symbol, TuringTapeMove.Stay)
        return self._transitions[frm]
        