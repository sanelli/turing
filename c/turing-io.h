#ifndef __TURING_IO__
#define __TURING_IO__

#include <turing-machine.h>

struct turing_machine* read_turing_machine_from_content(char* format, char* content);
struct turing_machine* read_turing_machine_from_file(char* format, char* filename);

#endif