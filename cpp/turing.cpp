#include <cstdlib>
#include <iostream>
#include <fstream>

#include "turing-tape.hpp"

int main(int argc, char* argv[])
{
    if(argc < 4)
    {
        std::cerr << "Usage: ./turing <format> <inputfile> <inputTape>" << std::endl;
        std::cerr << "Formats:" << std::endl;
        std::cerr << "   - toml: TOML file format" << std::endl;
        return EXIT_FAILURE;
    }

    std::ifstream stream(argv[2]);
    std::string program((std::istreambuf_iterator<char>(stream)), std::istreambuf_iterator<char>());

    std::cout << program << std::endl;

    return EXIT_SUCCESS;
}