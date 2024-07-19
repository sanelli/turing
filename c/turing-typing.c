#include <stdlib.h>
#include <string.h>

#include <turing-typing.h>

TURING_STATE turing_copy_state(TURING_STATE state)
{
    return strcpy((TURING_STATE)malloc(strlen(state) + 1), state);
}

void turing_free_state(TURING_STATE state)
{
    free(state);
}
