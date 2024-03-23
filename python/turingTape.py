from enum import Enum

from turingTyping import TuringSymbol

class TuringTapeMove(Enum):
    Stay = 0
    Left = 1
    Right = 2

class TuringTape():
    _emptySymbol : TuringSymbol
    _negativePositions: list[TuringSymbol]
    _positivePositions: list[TuringSymbol]
    _cursorPosition: int

    def __init__(self, emptySymbol:TuringSymbol) -> None:

        if len(emptySymbol) != 1:
            raise ValueError("emptySymbol mush have length 1")

        self._emptySymbol = emptySymbol
        self._cursorPosition = 0
        self._negativePositions = []
        self._positivePositions = []

    def setSymbol(self, symbol:TuringSymbol) -> None:
        if len(symbol) != 1:
            raise ValueError("emptySymbol mush have length 1")
        (positions, index) = self._getIndexForCurrentPosition()
        self._ensureTapeHasSpaceForIndex(positions, index)
        positions[index] = symbol
    
    def getSymbol(self) -> TuringSymbol:
        (positions, index) = self._getIndexForCurrentPosition()
        self._ensureTapeHasSpaceForIndex(positions, index)
        return positions[index]
    
    def moveLeft(self) -> None:
        self._cursorPosition = self._cursorPosition - 1

    def moveRight(self) -> None:
        self._cursorPosition = self._cursorPosition + 1

    def move(self, move:TuringTapeMove) -> None:
        if move == TuringTapeMove.Left:
            self.moveLeft()
        elif move == TuringTapeMove.Right:
            self.moveRight()
        elif move == TuringTapeMove.Stay:
            pass
        else:
            raise ValueError("Invalid or unknown move")
        
    def clear(self) -> None:
        self._cursorPosition = 0
        self._negativePositions = []
        self._positivePositions = []

    def initialize(self, initialSymbols:list[TuringSymbol], resetPosition:bool = True) -> None:
        self.clear()
        for symbol in initialSymbols:
            if len(symbol) != 1:
                raise ValueError("All symbols must have length 1")
            self.setSymbol(symbol)
            self.moveRight()
    
        if resetPosition:
            self._cursorPosition = 0

    def toString(self, separator:str = '|') -> str:
        negative = separator.join(self._negativePositions).strip()
        positive = separator.join(self._positivePositions).strip()
        separatorBetweenPositiveAndNegative = separator if negative.Length > 0 else ""
        result = negative + separatorBetweenPositiveAndNegative + positive
        if len(result) > 0 and not result.startswith(separator):
            result = separator + result
        if len(result) > 0 and not result.endswith(separator):
            result += separator

    def __str__(self) -> str:
        return self.toString()

    def _getIndexForCurrentPosition(self) -> tuple[list[TuringSymbol], int]:
        if self._cursorPosition >= 0:
            return (self._positivePositions, self._cursorPosition)
        else:
            return (self._negativePositions, -self._cursorPosition + 1)
        
    def _ensureTapeHasSpaceForIndex(self, positions:list[TuringSymbol], index:int) -> None:
        while len(positions) < index:
            positions.append(self._emptySymbol)