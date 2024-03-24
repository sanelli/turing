import tomllib
from turingMachine import TuringMachine
from turingTyping import TuringState, TuringSymbol
from turingTape import TuringTapeMove

def _turingMachineFromToml(content:str) -> TuringMachine:
    toml = tomllib.loads(content)
    states:set[str] = set(toml["States"])
    initialState:str = toml["InitialState"]
    finalStates:set[str] = set(toml["FinalStates"])
    symbols:set[str] = set(toml["Symbols"])
    emptySymbol:str = toml["EmptySymbol"]
    transitions:dict[tuple[TuringState,TuringSymbol],tuple[TuringState,TuringSymbol,TuringTapeMove]] = {}

    for transition in toml["Transitions"]:
        frm = (transition["State"], transition["Symbol"])
        to = (transition["NewState"], transition["NewSymbol"], _strToTuringTapeMove(transition["Move"]))
        transitions[frm] = to

    machine = TuringMachine(states, initialState, finalStates, symbols, emptySymbol, transitions)

    return machine

def _strToTuringTapeMove(move:str) -> TuringTapeMove:
    if move == "left":
        return TuringTapeMove.Left
    elif move == "right":
        return TuringTapeMove.Right
    elif move == "none":
        return TuringTapeMove.Stay
    else:
        raise ValueError(f"Uknown move '{move}'")

def turingMachineFromFormatString(format:str, content:str) -> TuringMachine:
    if format == "toml":
        return _turingMachineFromToml(content)
    
    raise ValueError(f"Unknown format '{format}'")