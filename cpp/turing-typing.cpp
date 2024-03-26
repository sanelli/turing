#include <string>
#include <set>
#include <stdexcept>
#include <sstream>

#include "turing-typing.hpp"

void turing::throwIfStateIsInvalid(turing::turing_state &state, const char *name)
{
    if (state.size() == 0)
    {
        std::stringstream stream;
        stream << name << " cannot be an empty string.";
        throw std::invalid_argument(stream.str());
    }
}

void turing::throwIfStateIsUnknown(turing::turing_state &state, std::set<turing::turing_state> &states, const char *name)
{
    if (states.find(state) == states.end())
    {
        std::stringstream stream;
        stream << name << " '" << state << "' does not belong to the list of states.";
        throw std::invalid_argument(stream.str());
    }
}

void turing::throwIfSymbolIsUnknown(turing::turing_symbol &symbol, std::set<turing::turing_symbol> &symbols, const char *name)
{
    if (symbols.find(symbol) == symbols.end())
    {
        std::stringstream stream;
        stream << name << " '" << symbol << "' does not belong to the list of states.";
        throw std::invalid_argument(stream.str());
    }
}