#ifndef __TURING_TAPE__
#define __TURING_TAPE__

#include <turing-typing.h>

enum turing_tape_move_direction 
{
    NONE,
    LEFT,
    RIGHT
};

struct turing_tape_entry
{
    TURING_SYMBOL symbol;
    struct turing_tape_entry* next;   
};

struct turing_tape
{
    struct turing_tape_entry* positive;
    struct turing_tape_entry* negative;
    int current_position;
    TURING_SYMBOL empty_symbol;
};

struct turing_tape* create_turing_tape(TURING_SYMBOL empty_symbol);
void free_turing_tape(struct turing_tape* tape);

void move_turing_tape(struct turing_tape* tape, enum turing_tape_move_direction direction);
void set_turing_tape_symbol(struct turing_tape* tape, TURING_SYMBOL symbol);
TURING_SYMBOL get_turing_tape_symbol(struct turing_tape* tape);
void clear_turing_tape(struct turing_tape* tape, TURING_SYMBOL* symbols, size_t number_of_symbols);

void print_turing_tape(struct turing_tape* tape, char separator);

#endif
