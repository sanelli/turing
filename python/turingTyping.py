import typing
import collections.abc

TuringSymbol = typing.TypeVar("TuringSymbol", bound=str)
TuringState = typing.TypeVar("TuringState", bound=str)

def raiseIfInvalidSymbol(symbol:TuringSymbol, name:str = "Symbol"):
    if len(symbol) != 1:
        raise ValueError(f"{name} must be a single character string but '{symbol}' has length {len(symbol)}")

def raiseIfUnknwodSymbol(symbol:TuringSymbol, symbols:collections.abc.Iterable[TuringSymbol],name:str = "Symbol"):
    if symbol not in symbols:
        raise ValueError(f"{name} '{symbol}' is not in the collection of valid symbols")

def raiseIfInvalidState(state:TuringState, name:str = "State"):
    if len(state) < 1:
        raise ValueError(f"{name} cannot be an empty string")
    
def raiseIfUnknwodState(state:TuringState, states:collections.abc.Iterable[TuringState],name:str = "State"):
    if state not in states:
        raise ValueError(f"{name} '{state}' is not in the collection of valid states")
