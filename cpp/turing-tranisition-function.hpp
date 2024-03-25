#pragma once

#include <map>

#include "turing-tape.hpp"

namespace turing
{
    struct turing_transition_function_from
    {
        std::string status;
        char symbol;
    };

    struct turing_transition_function_to
    {
        std::string status;
        char symbol;
        turing_tape_move move;
    };

    class turing_transition_function
    {
    private:
        std::string halt_state;
        std::map<turing_transition_function_from, turing_transition_function_to> transitions;

    public:
        turing_transition_function(std::string halt_state);
        void set(turing_transition_function_from frm, turing_transition_function_to to);
        turing_transition_function_to get(turing_transition_function_from frm) const;
    };

    inline bool operator<(const turing_transition_function_from &lhs, const turing_transition_function_from &rhs);
}