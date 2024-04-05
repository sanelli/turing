unit TuringTyping;
interface
    type 
        TTuringSymbol = char;
        TTuringState = string;
        TTapeMove = (None, Left, Right);
        TArrayOfStates = array of TTuringState;
        TSetOfSymbols = set of TTuringSymbol;
        TArrayOfSymbols = array of TTuringSymbol;
implementation
end.