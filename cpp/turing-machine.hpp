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
        template <typename TStateIterator, typename TSymbolIterator>
        turing_machine(
            TStateIterator first_state,
            TStateIterator last_state,
            turing_state initial_state,
            TStateIterator first_final_state,
            TStateIterator last_final_state,
            TSymbolIterator first_symbol,
            TSymbolIterator last_symbol,
            turing_symbol empty_symbol
            // TTransitionsIterator first_tranistion,
            // TTransitionsIterator last_transition
            ) : tape(' '), transitions("")
        {
            // Validate states
            std::set<turing_state> states;
            for (auto stateIt = first_state; stateIt != last_state; stateIt++)
            {
                turing_state state = *stateIt;
                throwIfStateIsInvalid(state);

                states.insert(state);
            }
            if (states.empty())
            {
                throw std::invalid_argument("The list of states cannot be empty");
            }

            // Validate initial state
            throwIfStateIsInvalid(initial_state, "Initial state");
            throwIfStateIsUnknown(initial_state, states, "Initial state");
            this->current_state = initial_state;
            this->initial_state = initial_state;

            // Validate final states
            for (auto finalStateIt = first_final_state; finalStateIt != last_final_state; finalStateIt++)
            {
                turing_state final_state = *finalStateIt;
                throwIfStateIsInvalid(final_state, "Final state");
                throwIfStateIsUnknown(final_state, states, "Final state");

                final_states.insert(final_state);
            }

            if (final_states.empty())
            {
                throw std::invalid_argument("The list of final states cannot be empty");
            }

            // Validate symbols
            for (auto symbolIt = first_symbol; symbolIt != last_symbol; symbolIt++)
            {
                turing_symbol symbol = *symbolIt;
                symbols.insert(symbol);
            }
            if (symbols.empty())
            {
                throw std::invalid_argument("The list of symbols cannot be empty");
            }

            throwIfSymbolIsUnknown(empty_symbol, symbols, "Empty symbol");

            // Initialize (again) tape and transitions
            tape = {empty_symbol};
            transitions = {*final_states.begin()};
        }

        void run();
    };
}