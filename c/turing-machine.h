#ifndef __TURING_MACHINE__
#define __TURING_MACHINE__

#include <stddef.h>
#include <turing-typing.h>
#include <turing-tape.h>
#include <turing-transition-function.h>

struct list_of_states_entry
{
    TURING_STATE state;
    struct list_of_states_entry* next;
};

struct list_of_symbols_entry
{
    TURING_SYMBOL symbol;
    struct list_of_symbols_entry* next;
};

struct turing_machine
{
    TURING_STATE initial_state;
    struct list_of_states_entry* states;
    struct list_of_symbols_entry* symbols;
    struct list_of_states_entry* final_states;
    TURING_STATE current_state;

    struct turing_tape* tape;
    struct turing_transition_function* transition_function;
};

struct turing_machine* create_turing_machine(
    TURING_STATE* states,
    size_t number_of_states,
    TURING_STATE initial_state,
    TURING_STATE* final_states,
    size_t number_of_final_states,
    TURING_SYMBOL* symbols,
    size_t number_of_symbols);
void free_turing_machine(struct turing_machine* tm);
void turing_machine_add_transition(
    struct turing_machine* tm,
    TURING_SYMBOL from_symbol,
    TURING_STATE from_state,
    TURING_SYMBOL to_symbol,
    TURING_STATE to_state,
    enum turing_tape_move_direction move);
void turing_machine_initialise(struct turing_machine* tm, TURING_SYMBOL* symbols, size_t number_of_symbols);

BOOL turing_machine_halted(struct turing_machine* tm);
BOOL turing_machine_step(struct turing_machine* tm);
void turing_machine_run(struct turing_machine* tm);

#endif
