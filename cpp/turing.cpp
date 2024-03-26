#include <cstdlib>
#include <iostream>
#include <fstream>
#include <memory>

#include "turing-tape.hpp"
#include "turing-io.hpp"
#include "turing-machine.hpp"

int main(int argc, char *argv[])
{
    if (argc < 4)
    {
        std::cerr << "Usage: ./turing <format> <inputfile> <inputTape>" << std::endl;
        std::cerr << "Formats:" << std::endl;
        std::cerr << "   - toml: TOML file format" << std::endl;
        return EXIT_FAILURE;
    }

    std::ifstream stream(argv[2]);
    std::string program((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());
    auto machine = turing::make_turing_machine(argv[1], program);
    std::string initial_tape(argv[3]);
    machine->clear(initial_tape.begin(), initial_tape.end());
    std::cout << "Initial tape: |" << machine->get_tape() << "|" << std::endl;
    machine->run();
    std::cout << "Final tape: |" << machine->get_tape() << "|" << std::endl;
    std::cout << "Final state: '" << machine->get_current_state() << "'" << std::endl;

    return EXIT_SUCCESS;
}