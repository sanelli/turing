module turing_machine_io;

import std.stdio;
import std.format;
import std.file;
import std.algorithm.searching;

import toml;

import turing_machine;
import turing_typing;
import turing_transitions;

TuringMachine loadTuringMachineFromFile(string format, string filename)
{
    string program = readText(filename);
    return loadTuringMachineFromString(format, program);
}

TuringMachine loadTuringMachineFromString(string format, string program)
{
    if (format == "toml")
    {
        return loadTuringMachineFromTomlString(program);
    }

    return null;
}

package string[] toStringArray(TOMLValue[] values)
{
    string[] result;
    foreach (TOMLValue value; values)
    {
        result ~= value.str();
    }

    return result;
}

package char[] toCharArray(TOMLValue[] values)
{
    char[] result;
    foreach (TOMLValue value; values)
    {
        result ~= value.str()[0];
    }

    return result;
}

package TuringMachine loadTuringMachineFromTomlString(string program)
{
    auto transitionFunction = new TransitionFunction();

    TOMLDocument doc = parseTOML(program);
    TuringState[] states = toStringArray(doc["States"].array());
    TuringState initialState = doc["InitialState"].str();
    if (!canFind(states, initialState))
    {
        "ERROR: Invalid initial state '%s'".format(initialState).writeln;
        return null;
    }

    TuringState[] haltStates = toStringArray(doc["FinalStates"].array());
    foreach (string haltState; haltStates)
    {
        if (!canFind(states, haltState))
        {
            "ERROR: Invalid final state '%s'".format(haltState).writeln;
            return null;
        }
    }

    TuringSymbol[] symbols = toCharArray(doc["Symbols"].array());
    TuringSymbol emptySymbol = doc["EmptySymbol"].str()[0];
    if (!canFind(symbols, emptySymbol))
    {
        "ERROR: Invalid empty symbol '%s'".format(emptySymbol).writeln;
        return null;
    }

    auto transitions = doc["Transitions"].array();
    foreach (TOMLValue transitionValue; transitions)
    {
        TuringSymbol symbol = transitionValue["Symbol"].str()[0];
        if (!canFind(symbols, symbol))
        {
            "ERROR: Invalid symbol '%s'".format(symbol).writeln;
            return null;
        }

        TuringState state = transitionValue["State"].str();
        if (!canFind(states, state))
        {
            "ERROR: Invalid state '%s'".format(state).writeln;
            return null;
        }

        TuringSymbol newSymbol = transitionValue["NewSymbol"].str()[0];
        if (!canFind(symbols, newSymbol))
        {
            "ERROR: Invalid target symbol '%s'".format(newSymbol).writeln;
            return null;
        }

        TuringState newState = transitionValue["NewState"].str();
        if (!canFind(states, newState))
        {
            "ERROR: Invalid target state '%s'".format(newState).writeln;
            return null;
        }

        TuringMoveDirection move = fromString(transitionValue["Move"].str());

        auto from = new TransitionFunctionFrom(symbol, state);
        auto to = new TransitionFunctionTo(newSymbol, newState, move);
        transitionFunction.add(from, to);
    }

    auto machine = new TuringMachine(emptySymbol, initialState, haltStates, transitionFunction);
    return machine;
}


unittest // substitute
{
    string program = "States = [ \"replace\", \"halt\" ]\n" ~
       "InitialState = \"replace\"\n" ~
       "FinalStates = [ \"halt\" ]\n" ~
       "Symbols = [ \" \", \"a\", \"b\" ]\n" ~
       "EmptySymbol = \" \"\n" ~
       "[[Transitions]]\n" ~
       "State = \"replace\"\n" ~
       "Symbol = \"a\"\n" ~
       "NewState = \"replace\"\n" ~
       "NewSymbol = \"b\"\n" ~
       "Move = \"right\"\n" ~
       "[[Transitions]]\n" ~
       "State = \"replace\"\n" ~
       "Symbol = \"b\"\n" ~
       "NewState = \"replace\"\n" ~
       "NewSymbol = \"a\"\n" ~
       "Move = \"right\"\n" ~
       "[[Transitions]]\n" ~
       "State = \"replace\"\n" ~
       "Symbol = \" \"\n" ~
       "NewState = \"halt\"\n" ~
       "NewSymbol = \" \"\n" ~
       "Move = \"right\"\n";
    string input = "abba";

    auto machine = loadTuringMachineFromString("toml", program);
    machine.clear(input);
    machine.run();
    assert(machine.getTape() == "|b|a|a|b| |");
    assert(machine.getCurrentState() == "halt");
}