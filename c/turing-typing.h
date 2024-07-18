#ifndef __TURING_TYPING__
#define __TURING_TYPING__

#define TURING_SYMBOL char
#define TURING_STATE char*

TURING_STATE turing_copy_state(TURING_STATE state);
void turing_free_state(TURING_STATE state);

#endif
