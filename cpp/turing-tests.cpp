#include <catch2/catch_test_macros.hpp>

#include "turing-io.hpp"
#include "turing-machine.hpp"

TEST_CASE("Substitute", "[turing][substitute]")
{
    std::string program = {
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
        "Move = \"right\"\n"};
    auto machine = turing::make_turing_machine("toml", program);
    std::string initial_tape("abba");
    machine->clear(initial_tape.begin(), initial_tape.end());
    REQUIRE(machine->get_tape() == "|a|b|b|a|");
    machine->run();
    REQUIRE(machine->get_tape() == "|b|a|a|b|");
    REQUIRE(machine->get_current_state() == "halt");
}
