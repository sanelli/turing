#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <turing-machine.h>
#include <turing-io.h>
#include <turing-tape.h>

int main(int argc, char *argv[])
{
   if (argc != 4)
   {
      printf("Usage: %s <format> <input-file> <input-tape>", argv[0]);
      printf("Formats:");
      printf("  - toml: The TOML format of the file desribing the turing machine");
      return EXIT_FAILURE;
   }

   struct turing_machine *tm = read_turing_machine_from_file(argv[1], argv[2]);
   if(tm == NULL)
   {
      return EXIT_FAILURE;
   }
   
   turing_machine_initialise(tm, argv[3], strlen(argv[3]));
   printf("Initial tape: ");
   print_turing_tape(tm->tape, '|');
   printf("\n");

   // turing_machine_run(tm);

   printf("Final tape: ");
   print_turing_tape(tm->tape, '|');
   printf("\n");
   printf("Final state: %s\n", tm->current_state);
   free_turing_machine(tm);
   return EXIT_SUCCESS;
}
