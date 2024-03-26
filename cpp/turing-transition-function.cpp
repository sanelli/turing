#include <map>
#include <stdexcept>

#include "turing-tape.hpp"
#include "turing-transition-function.hpp"

using namespace turing;

turing::turing_transition_function::turing_transition_function(std::string halt_state)
{
    this->halt_state = halt_state;
}

void turing_transition_function::set(turing_transition_function_from frm, turing_transition_function_to to)
{
    if (transitions.find(frm) != transitions.end())
    {
        throw std::invalid_argument("Transition already added");
    }

    transitions[frm] = to;
}

turing_transition_function_to turing_transition_function::get(turing_transition_function_from frm) const
{
    auto transition = transitions.find(frm);
    if (transition == transitions.end())
    {
        return turing_transition_function_to{halt_state, frm.symbol, turing_tape_move::None};
    }

    return transition->second;
}

inline bool turing::operator<(const turing_transition_function_from &lhs, const turing_transition_function_from &rhs)
{
    return (lhs.status < rhs.status) || (lhs.status == rhs.status && lhs.symbol < rhs.symbol);
}