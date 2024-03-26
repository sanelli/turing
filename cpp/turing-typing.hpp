#pragma once

#include <string>
#include <set>

namespace turing
{
    using turing_symbol = char;
    using turing_state = std::string;

    void throwIfStateIsInvalid(turing_state &state, const char* name = "State");
    void throwIfStateIsUnknown(turing_state &state, std::set<turing_state> &states, const char* name = "State");
    void throwIfSymbolIsUnknown(turing_symbol &symbol, std::set<turing_symbol> &symbols, const char* name = "Symbol");
}