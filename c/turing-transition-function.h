#ifndef __TURING_TRANSITION_FUNCTION__
#define __TURING_TRANSITION_FUNCTION__

#include <turing-tape.h>
#include <turing-typing.h>

struct turing_transition_function_entry
{
    TURING_SYMBOL from_symbol;
    TURING_STATE from_state;
    TURING_SYMBOL to_symbol;
    TURING_STATE to_state;
    enum turing_tape_move_direction move;
    struct turing_transition_function_entry* next;
};

struct turing_transition_function
{
    TURING_STATE halt_state;
    struct turing_transition_function_entry* transition;
};

struct turing_transition_function* create_turing_transition_function(TURING_STATE halt_state);
void free_turing_transition_function(struct turing_transition_function* ttf);
void turing_transition_function_append(
    struct turing_transition_function* ttf,
    TURING_SYMBOL from_symbol,
    TURING_STATE from_state,
    TURING_SYMBOL to_symbol,
    TURING_STATE to_state,
    enum turing_tape_move_direction move);
struct turing_transition_function_entry turing_transition_function_find(struct turing_transition_function* ttf, TURING_SYMBOL from_symbol, TURING_STATE from_state);

void turing_transition_function_print(struct turing_transition_function *ttf);

#endif
