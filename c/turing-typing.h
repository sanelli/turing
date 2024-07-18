#ifndef __TURING_TYPING__
#define __TURING_TYPING__

#define TURING_SYMBOL char
#define TURING_STATE char*

#define BOOL int

#define FALSE 0
#define TRUE 1

TURING_STATE turing_copy_state(TURING_STATE state);
void turing_free_state(TURING_STATE state);

#endif
