#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <turing-io.h>
#include <turing-typing.h>

BOOL test_substitute(void);
char *get_success(BOOL success);
BOOL is_expected(struct turing_machine *tm, char *tape, char *status);

int main(void)
{
   // Substitute test
   BOOL success = TRUE;
   printf("* Substitute: ");
   BOOL substitute_success = test_substitute();
   success = success && substitute_success;
   printf("%s\n", get_success(substitute_success));

   // All done
   switch (success)
   {
   case FALSE:
      printf("Tests failed.\n");
      break;
   case TRUE:
      printf("All tests passed.\n");
      break;
   }

   return success ? EXIT_SUCCESS : EXIT_FAILURE;
}

BOOL test_substitute(void)
{
   char *content =
       "States = [ \"replace\", \"halt\" ]\n"
       "InitialState = \"replace\"\n"
       "FinalStates = [ \"halt\" ]\n"
       "Symbols = [ \" \", \"a\", \"b\" ]\n"
       "EmptySymbol = \" \"\n"
       "[[Transitions]]\n"
       "State = \"replace\"\n"
       "Symbol = \"a\"\n"
       "NewState = \"replace\"\n"
       "NewSymbol = \"b\"\n"
       "Move = \"right\"\n"
       "[[Transitions]]\n"
       "State = \"replace\"\n"
       "Symbol = \"b\"\n"
       "NewState = \"replace\"\n"
       "NewSymbol = \"a\"\n"
       "Move = \"right\"\n"
       "[[Transitions]]\n"
       "State = \"replace\"\n"
       "Symbol = \" \"\n"
       "NewState = \"halt\"\n"
       "NewSymbol = \" \"\n"
       "Move = \"right\"\n";
   struct turing_machine *tm = read_turing_machine_from_content("toml", content);
   turing_machine_initialise(tm, "abba", 4);
   turing_machine_run(tm);
   return is_expected(tm, "baab ", "halt");
}

char *get_success(BOOL success)
{
   switch (success)
   {
   case TRUE:
      return "SUCCESS";
   case FALSE:
      return "FAIL";
   }

   return "UKNOWN";
}

BOOL is_expected(struct turing_machine *tm, char *tape, char *status)
{
   if (strcmp(status, tm->current_state) != 0)
   {
      return FALSE;
   }

   char buffer[1024];
   turing_tape_to_buffer(tm->tape, buffer, 1024);

   if (strcmp(tape, buffer) != 0)
   {
      return FALSE;
   }

   return TRUE;
}
