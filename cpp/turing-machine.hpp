#pragma once

#include <set>
#include <stdexcept>
#include <string>

#include "turing-transition-function.hpp"
#include "turing-tape.hpp"
#include "turing-typing.hpp"

namespace turing
{
    class turing_machine
    {
        turing_state initial_state;
        std::set<turing_state> final_states;
        std::set<turing_symbol> symbols;
        turing_state current_state;
        turing_tape tape;
        turing_transition_function transitions;

    public:
        template <typename TStateIterator>
        turing_machine(
            TStateIterator first_state,
            TStateIterator last_state,
            turing_state initial_state
            // TStateIterator first_final_state,
            // TStateIterator last_final_state,
            // TSymbolIterator first_symbol,
            // TSymbolIterator last_symbol,
            // turing_symbol initial_symbol,
            // TTransitionsIterator first_tranistion,
            // TTransitionsIterator last_transition
            ) : tape(' '), transitions("")
        {
            // Validate states
            std::set<turing_state> states;
            for (auto stateIt = first_state; stateIt != last_state; stateIt++)
            {
                turing_state state = *stateIt;
                if (state == "")
                {
                    throw std::invalid_argument("State cannot be an empty string");
                }

                states.insert(state);
            }

            if (initial_state == "")
            {
                throw std::invalid_argument("Initial state cannot be empty");
            }

            if (states.find(initial_state) == states.end())
            {
                throw std::invalid_argument("Initial state does not belong to the set of accepted states");
            }

            this->current_state = initial_state;
            this->initial_state = initial_state;

            tape = {' '};
            transitions = {""};
        }

        void run();
    };
}