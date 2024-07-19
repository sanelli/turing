#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <toml-c.h>

#include <turing-io.h>
#include <turing-tape.h>
#include <turing-typing.h>
#include <turing-machine.h>

struct turing_machine *read_turing_machine_from_toml_content(char *content);

struct turing_machine *read_turing_machine_from_content(char *format, char *content)
{
    if (strcmp(format, "toml") == 0)
    {
        return read_turing_machine_from_toml_content(content);
    }

    return NULL;
}

struct turing_machine *read_turing_machine_from_file(char *format, char *filename)
{
    FILE *file = fopen(filename, "r");
    if (file == NULL)
    {
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    char *content = (char *)malloc(size + 1);
    char buffer[1024];

    memset(content, 0, size + 1);
    memset(buffer, 0, 1024);
    fseek(file, 0, SEEK_SET);
    while (fgets(buffer, 1024, file) != NULL)
    {
        strcat(content, buffer);
    }

    struct turing_machine *tm = read_turing_machine_from_content(format, content);

    fclose(file);
    free(content);
    return tm;
}

struct turing_machine *read_turing_machine_from_toml_content(char *content)
{
    char errbuf[1024];
    struct toml_table_t *root = toml_parse(content, errbuf, sizeof(errbuf));
    if (!root)
    {
        printf("Cannot load TOML content\n");
        return NULL;
    }

    // States
    struct toml_array_t *toml_states = toml_table_array(root, "States");
    int number_of_states = toml_array_len(toml_states);
    TURING_STATE *states = (TURING_STATE *)malloc(sizeof(TURING_STATE) * number_of_states);
    memset(states, 0, sizeof(TURING_STATE) * number_of_states);
    for (int index = 0; index < number_of_states; ++index)
    {
        struct toml_value_t state_toml = toml_array_string(toml_states, index);
        states[index] = turing_copy_state(state_toml.u.s);
        free(state_toml.u.s);
    }

    // Initial state
    struct toml_value_t initial_state_toml = toml_table_string(root, "InitialState");
    TURING_STATE initial_state = turing_copy_state(initial_state_toml.u.s);
    free(initial_state_toml.u.s);

    // Final states
    struct toml_array_t *toml_final_states = toml_table_array(root, "FinalStates");
    int number_of_final_states = toml_array_len(toml_final_states);
    TURING_STATE *final_states = (TURING_STATE *)malloc(sizeof(TURING_STATE) * number_of_final_states);
    memset(final_states, 0, sizeof(TURING_STATE) * number_of_final_states);
    for (int index = 0; index < number_of_final_states; ++index)
    {
        struct toml_value_t state_toml = toml_array_string(toml_final_states, index);
        final_states[index] = turing_copy_state(state_toml.u.s);
        free(state_toml.u.s);
    }

    // Symbols
    struct toml_array_t *toml_symbols = toml_table_array(root, "Symbols");
    int number_of_symbols = toml_array_len(toml_symbols);
    TURING_SYMBOL *symbols = (TURING_SYMBOL *)malloc(sizeof(TURING_SYMBOL) * number_of_symbols);
    memset(symbols, 0, sizeof(TURING_SYMBOL) * number_of_symbols);
    for (int index = 0; index < number_of_symbols; ++index)
    {
        struct toml_value_t symbol_toml = toml_array_string(toml_symbols, index);
        symbols[index] = symbol_toml.u.s[0];
        free(symbol_toml.u.s);
    }

    // Empty symbol
    struct toml_value_t empty_symbol_toml = toml_table_string(root, "EmptySymbol");
    TURING_SYMBOL empty_symbol = empty_symbol_toml.u.s[0];
    free(empty_symbol_toml.u.s);

    // Create the turing machine
    struct turing_machine *tm = create_turing_machine(
        states,
        number_of_states,
        initial_state,
        final_states,
        number_of_final_states,
        symbols,
        number_of_symbols,
        empty_symbol);

    // Add transformaiton function
    struct toml_array_t *transitions_table_toml = toml_table_array(root, "Transitions");
    int number_of_transactions = toml_array_len(transitions_table_toml);
    for (int index = 0; index < number_of_transactions; ++index)
    {
        struct toml_table_t *transaction_toml = toml_array_table(transitions_table_toml, index);
        struct toml_value_t from_state = toml_table_string(transaction_toml, "State");
        struct toml_value_t from_symbol = toml_table_string(transaction_toml, "Symbol");
        struct toml_value_t to_state = toml_table_string(transaction_toml, "NewState");
        struct toml_value_t to_symbol = toml_table_string(transaction_toml, "NewSymbol");
        struct toml_value_t move = toml_table_string(transaction_toml, "Move");

        turing_machine_add_transition(
            tm,
            from_symbol.u.s[0],
            from_state.u.s,
            to_symbol.u.s[0],
            to_state.u.s,
            turing_tape_move_direction_from_string(move.u.s));

        free(from_state.u.s);
        free(from_symbol.u.s);
        free(to_state.u.s);
        free(to_symbol.u.s);
        free(move.u.s);
    }

    // Free everything
    toml_free(root);
    for (int index = 0; index < number_of_states; ++index)
    {
        turing_free_state(states[index]);
    }
    free(states);
    turing_free_state(initial_state);
    for (int index = 0; index < number_of_states; ++index)
    {
        turing_free_state(final_states[index]);
    }
    free(final_states);
    free(symbols);

    // All done
    return tm;
}
