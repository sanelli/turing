#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include <turing-typing.h>
#include <turing-tape.h>
#include <turing-transition-function.h>
#include <turing-machine.h>

void free_turing_machine_list_of_status(struct list_of_states_entry* head)
{
    struct list_of_states_entry* cursor = head;
    while(cursor != NULL)
    {
        struct list_of_states_entry* current = cursor;
        cursor = cursor->next;
        turing_free_state(cursor->state);
        free(current);
    }
}

void free_turing_machine_list_of_symbols(struct list_of_symbols_entry* head)
{
    struct list_of_symbols_entry* cursor = head;
    while(cursor != NULL)
    {
        struct list_of_symbols_entry* current = cursor;
        cursor = cursor->next;
        free(current);
    }
}

struct turing_machine *create_turing_machine(
    TURING_STATE *states,
    size_t number_of_states,
    TURING_STATE initial_state,
    TURING_STATE *final_states,
    size_t number_of_final_states,
    TURING_SYMBOL *symbols,
    size_t number_of_symbols)
{
    // IMPLEMENT ME
    return NULL;
}

void free_turing_machine(struct turing_machine *tm)
{
    free(tm);
    free_turing_tape(tm->tape);
    free_turing_transition_function(tm->transition_function);
    free_turing_machine_list_of_status(tm->states);
    free_turing_machine_list_of_status(tm->final_states);
    free_turing_machine_list_of_symbols(tm->symbols);
}

void turing_machine_add_transition(
    struct turing_machine *tm,
    TURING_SYMBOL from_symbol,
    TURING_STATE from_state,
    TURING_SYMBOL to_symbol,
    TURING_STATE to_state,
    enum turing_tape_move_direction move)
{
    turing_transition_function_append(tm->transition_function, from_symbol, from_state, to_symbol, to_state, move);
}

void turing_machine_initialise(struct turing_machine *tm, TURING_SYMBOL *symbols, size_t number_of_symbols)
{
    clear_turing_tape(tm->tape, symbols, number_of_symbols);
}

BOOL turing_machine_halted(struct turing_machine *tm)
{
    struct list_of_states_entry *cursor = tm->final_states;
    while (cursor != NULL)
    {
        if (strcmp(tm->current_state, cursor->state) == 0)
        {
            return TRUE;
        }
        cursor = cursor->next;
    }

    return FALSE;
}

BOOL turing_machine_step(struct turing_machine *tm)
{
    if (turing_machine_halted(tm))
    {
        return FALSE;
    }

    TURING_SYMBOL current_symbol = get_turing_tape_symbol(tm->tape);
    struct turing_transition_function_entry transition = turing_transition_function_find(tm->transition_function, current_symbol, tm->current_state);

    tm->current_state = transition.to_state;
    set_turing_tape_symbol(tm->tape, transition.to_symbol);
    move_turing_tape(tm->tape, transition.move);

    return TRUE;
}

void turing_machine_run(struct turing_machine *tm)
{
    while (!turing_machine_step(tm))
        ;
}
