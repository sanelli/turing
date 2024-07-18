#include <stdlib.h>
#include <string.h>

#include <turing-tape.h>
#include <turing-typing.h>
#include <turing-transition-function.h>

struct turing_transition_function_entry *turing_transition_function_entry_append(
    struct turing_transition_function_entry *head,
    TURING_SYMBOL from_symbol,
    TURING_STATE from_state,
    TURING_SYMBOL to_symbol,
    TURING_STATE to_state,
    enum turing_tape_move_direction move)
{
    struct turing_transition_function_entry *entry = (struct turing_transition_function_entry *)malloc(sizeof(struct turing_transition_function_entry));
    entry->from_symbol = from_symbol;
    entry->from_state = turing_copy_state(from_state);
    entry->to_symbol = to_symbol;
    entry->to_state = turing_copy_state(to_state);
    entry->move = move;
    entry->next = NULL;

    if (head == NULL)
    {
        return entry;
    }
    else
    {
        struct turing_transition_function_entry *cursor = head;
        while (cursor->next != NULL)
        {
            cursor = cursor->next;
        }
        cursor->next = entry;
        return head;
    }
}

void free_turing_transition_function_entry(struct turing_transition_function_entry *head)
{
    struct turing_transition_function_entry *cursor = head;
    while (cursor != NULL)
    {
        struct turing_transition_function_entry *current = cursor;
        cursor = cursor->next;
        turing_free_state(cursor->from_state);
        turing_free_state(cursor->to_state);
        free(current);
    }
}

struct turing_transition_function_entry *turing_transition_function_entry_find(struct turing_transition_function_entry *head, TURING_SYMBOL from_symbol, TURING_STATE from_state)
{
    struct turing_transition_function_entry *cursor = head;
    while (cursor != NULL)
    {
        if (cursor->from_symbol == from_symbol && strcmp(cursor->from_state, from_state) == 0)
        {
            return cursor;
        }
    }

    return NULL;
}

struct turing_transition_function *create_turing_transition_function(TURING_STATE halt_state)
{
    struct turing_transition_function *ttf = (struct turing_transition_function *)malloc(sizeof(struct turing_transition_function));
    ttf->halt_state = turing_copy_state(halt_state);
    ttf->transition = NULL;
    return ttf;
}

void free_turing_transition_function(struct turing_transition_function *ttf)
{
    free_turing_transition_function_entry(ttf->transition);
    turing_free_state(ttf->halt_state);
    free(ttf);
}

void turing_transition_function_append(
    struct turing_transition_function *ttf,
    TURING_SYMBOL from_symbol,
    TURING_STATE from_state,
    TURING_SYMBOL to_symbol,
    TURING_STATE to_state,
    enum turing_tape_move_direction move)
{
    if (turing_transition_function_entry_find(ttf->transition, from_symbol, from_state) != NULL)
    {
        ttf->transition = turing_transition_function_entry_append(ttf->transition, from_symbol, from_state, to_symbol, to_state, move);
    }
}

struct turing_transition_function_entry turing_transition_function_find(struct turing_transition_function *ttf, TURING_SYMBOL from_symbol, TURING_STATE from_state)
{
    struct turing_transition_function_entry result;
    result.next = NULL;

    struct turing_transition_function_entry *entry = turing_transition_function_entry_find(ttf->transition, from_symbol, from_state);
    if(entry != NULL)
    {
        result = *entry;
    } else {
        result.from_state = from_state;
        result.from_symbol = from_symbol;
        result.to_state = ttf->halt_state;
        result.to_symbol = from_symbol;
        result.move = NONE;
    }

    result.next = NULL;
    return result;
}
