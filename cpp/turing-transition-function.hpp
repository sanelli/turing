#pragma once

#include <map>

#include "turing-tape.hpp"
#include "turing-typing.hpp"

namespace turing
{
    struct turing_transition_function_from
    {
        turing_state state;
        turing_symbol symbol;
    };

    struct turing_transition_function_to
    {
        turing_state state;
        turing_symbol symbol;
        turing_tape_move move;
    };

    class turing_transition_function
    {
    private:
        turing_state halt_state;
        std::map<turing_transition_function_from, turing_transition_function_to> transitions;

    public:
        turing_transition_function(turing_state halt_state);
        void set(turing_transition_function_from frm, turing_transition_function_to to);
        turing_transition_function_to get(turing_transition_function_from frm) const;
        bool empty() const;
    };

    inline bool operator<(const turing_transition_function_from &lhs, const turing_transition_function_from &rhs);
}