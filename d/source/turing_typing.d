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