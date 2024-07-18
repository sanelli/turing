#include <stdlib.h>
#include <stdio.h>

#include <turing-typing.h>
#include <turing-tape.h>

struct turing_tape_entry *turing_tape_entry_append(struct turing_tape_entry *head, TURING_SYMBOL symbol)
{
    struct turing_tape_entry *entry = (struct turing_tape_entry *)malloc(sizeof(struct turing_tape_entry));
    entry->symbol = symbol;
    entry->next = NULL;

    if (head == NULL)
    {
        return entry;
    }
    else
    {
        struct turing_tape_entry *cursor = head;
        while (cursor->next != NULL)
        {
            cursor = cursor->next;
        }
        cursor->next = entry;
        return head;
    }
}

int turing_tape_entry_length(struct turing_tape_entry *entry)
{
    int length = 0;
    while (entry != NULL)
    {
        ++length;
        entry = entry->next;
    }
    return length;
}

void turing_tape_entry_set_at(struct turing_tape_entry *head, int position, TURING_SYMBOL symbol)
{
    struct turing_tape_entry *cursor = head;
    int index = 0;
    while (index != position)
    {
        cursor = cursor->next;
        ++index;
    }

    cursor->symbol = symbol;
}

TURING_SYMBOL turing_tape_entry_get_at(struct turing_tape_entry *head, int position)
{
    struct turing_tape_entry *cursor = head;
    int index = 0;
    while (index != position)
    {
        cursor = cursor->next;
        ++index;
    }

    return cursor->symbol;
}

struct turing_tape *create_turing_tape(TURING_SYMBOL empty_symbol)
{
    struct turing_tape *tape = (struct turing_tape *)malloc(sizeof(struct turing_tape));
    tape->positive = NULL;
    tape->negative = NULL;
    tape->current_position = 0;
    tape->empty_symbol = empty_symbol;
    return tape;
}

void free_turing_tape(struct turing_tape *tape)
{
    clear_turing_tape(tape, NULL, 0);
}

void ensure_turing_tapes_sizes(struct turing_tape *tape)
{
    if (tape->current_position >= 0)
    {
        while (tape->current_position <= turing_tape_entry_length(tape->positive))
        {
            turing_tape_entry_append(tape->positive, tape->empty_symbol);
        }
    }
    else
    {
        while (tape->current_position <= turing_tape_entry_length(tape->negative) + 1)
        {
            turing_tape_entry_append(tape->negative, tape->empty_symbol);
        }
    }
}

void set_turing_tape_symbol(struct turing_tape *tape, TURING_SYMBOL symbol)
{
    ensure_turing_tapes_sizes(tape);
    if (tape->current_position >= 0)
    {
        turing_tape_entry_set_at(tape->positive, tape->current_position, symbol);
    }
    else
    {
        turing_tape_entry_set_at(tape->negative, -tape->current_position + 1, symbol);
    }
}

TURING_SYMBOL get_turing_tape_symbol(struct turing_tape *tape)
{
    ensure_turing_tapes_sizes(tape);
    if (tape->current_position >= 0)
    {
        return turing_tape_entry_get_at(tape->positive, tape->current_position);
    }
    else
    {
        return turing_tape_entry_get_at(tape->negative, -tape->current_position + 1);
    }
}

void move_turing_tape(struct turing_tape *tape, enum turing_tape_move_direction direction)
{
    switch (direction)
    {
    case NONE:
        break;
    case LEFT:
        tape->current_position--;
        break;
    case RIGHT:
        tape->current_position++;
        break;
    }
}

void print_turing_tape(struct turing_tape *tape, char separator)
{
    // Print negative
    printf("%c", separator);
    for (int index = 0; index < turing_tape_entry_length(tape->negative); ++index)
    {
        printf("%c", turing_tape_entry_get_at(tape->negative, index));
        printf("%c", separator);
    }

    // Print positive
    for (struct turing_tape_entry *cursor = tape->positive; cursor != NULL; cursor = cursor->next)
    {
        printf("%c", cursor->symbol);
        printf("%c", separator);
    }
}

void clear_turing_tape(struct turing_tape *tape, TURING_SYMBOL *symbols, size_t number_of_symbols)
{
    while (tape->positive != NULL)
    {
        struct turing_tape_entry *next = tape->positive->next;
        free(tape->positive);
        tape->positive = next;
    }
    tape->positive = NULL;

    while (tape->negative != NULL)
    {
        struct turing_tape_entry *next = tape->negative->next;
        free(tape->negative);
        tape->negative = next;
    }
    tape->negative = NULL;

    if (symbols != NULL && number_of_symbols > 0)
    {
        for (size_t index = (size_t)0; index < number_of_symbols; ++index)
        {
            set_turing_tape_symbol(tape, symbols[index]);
        }
    }

    tape->current_position = 0;
}
