module turing_typing;

import std;
import optional;

alias TuringSymbol = char;
alias TuringState = string;

struct Success { }
alias SuccessResponse = Optional!Success;

enum TuringMoveDirection 
{
    Left,
    Right,
    None,
}

TuringMoveDirection fromString(string s)
{
    if(s == "left") {
        return TuringMoveDirection.Left;
    } else  if(s == "right") {
        return TuringMoveDirection.Right;
    } else {
        return TuringMoveDirection.None;
    }
}