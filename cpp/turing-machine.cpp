#include "turing-tape.hpp"
#include "turing-transition-function.hpp"
#include "turing-machine.hpp"

using namespace turing;

void turing_machine::run()
{
    while (!halted())
    {
        step();
    }
}

void turing_machine::step()
{
    if (halted())
    {
        return;
    }

    turing::turing_transition_function_from from{current_state, tape.get()};
    auto to = transitions.get(from);
    this->current_state = to.state;
    this->tape.set(to.symbol);
    this->tape.move(to.move);
}

bool turing_machine::halted() const
{
    return final_states.find(current_state) != final_states.end();
}

turing_state turing_machine::get_current_state() const
{
    return current_state;
}

std::string turing_machine::get_tape() const
{
    return tape.str();
}
