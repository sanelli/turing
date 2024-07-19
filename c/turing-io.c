#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <turing-machine.h>

struct turing_machine* read_turing_machine_from_toml_content(char* content);

struct turing_machine* read_turing_machine_from_content(char* format, char* content)
{
    if(strcmp(format, "toml") == 0)
    {
        read_turing_machine_from_toml_content(content);
    }

    return NULL;
}

struct turing_machine* read_turing_machine_from_file(char* format, char* filename)
{
    FILE* file = fopen(filename, "r");
    if(file == NULL)
    {
        return NULL;
    }
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    char* content = (char*)malloc(size + 1);
    char buffer[1024];

    memset(content, 0, size + 1);
    memset(buffer, 0, 1024);
    while(fgets(buffer, 1024, file) != NULL)
    {
        strcat(content, buffer);
    }

    struct turing_machine* tm = read_turing_machine_from_content(format, content);

    fclose(file);
    free(content);
    return tm;
}

struct turing_machine* read_turing_machine_from_toml_content(char* content)
{
    return NULL;
}
