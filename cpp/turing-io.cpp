#include <memory>
#include <string>
#include <sstream>
#include <stdexcept>
#include <vector>

#include <toml.hpp>

#include "turing-machine.hpp"
#include "turing-io.hpp"

using namespace turing;

std::unique_ptr<turing::turing_machine> turing::make_turing_machine(const std::string &format, const std::string &machine)
{
    if (format == "toml")
    {
        return make_turing_machine_from_toml(machine);
    }

    throw std::invalid_argument("Unknown format");
}

std::unique_ptr<turing::turing_machine> turing::make_turing_machine_from_toml(const std::string &machine)
{
    std::istringstream iss{machine};
    const auto data = toml::parse(iss);

    auto states = toml::find<std::vector<turing_state>>(data, "States");
    auto initial_state = toml::find<std::string>(data, "InitialState");
    auto final_states = toml::find<std::vector<turing_state>>(data, "FinalStates");
    auto string_symbols = toml::find<std::vector<std::string>>(data, "Symbols");
    auto empty_symbol = toml::find<std::string>(data, "EmptySymbol");

    std::vector<turing_symbol> symbols;
    for (auto symbolIt = string_symbols.begin(); symbolIt != string_symbols.end(); symbolIt++)
    {
        auto string_symbol = *symbolIt;

        if (string_symbol.size() != 1)
        {
            std::stringstream stream;
            stream << "Symbol '" << string_symbol << "' is invalid: symbol length must be exactly one character";
            throw std::invalid_argument(stream.str());
        }
        symbols.push_back(string_symbol[0]);
    }

    if (empty_symbol.size() != 1)
    {
        throw std::invalid_argument("EmptySymbol must be exactly one character long");
    }

    auto transition_tables = toml::find<std::vector<toml::table>>(data, "Transitions");
    std::vector<std::pair<turing_transition_function_from, turing_transition_function_to>> transitions;
    for (auto tableIt = transition_tables.begin(); tableIt != transition_tables.end(); tableIt++)
    {
        const auto &tbl = *tableIt;
        auto state = tbl.at("State").as_string().str;
        auto symbol_str = tbl.at("Symbol").as_string().str;
        auto new_state = tbl.at("NewState").as_string().str;
        auto new_symbol_str = tbl.at("NewSymbol").as_string().str;
        auto move_str = tbl.at("Move").as_string().str;

        if (symbol_str.size() != 1)
        {
            std::stringstream stream;
            stream << "Origin symbol '" << symbol_str << "' must have length 1.";
            throw std::invalid_argument(stream.str());
        }

        if (new_symbol_str.size() != 1)
        {
            std::stringstream stream;
            stream << "Target symbol '" << new_symbol_str << "' must have length 1.";
            throw std::invalid_argument(stream.str());
        }

        turing_tape_move move;
        if (move_str == "left")
        {
            move = turing_tape_move::Left;
        }
        else if (move_str == "right")
        {
            move = turing_tape_move::Right;
        }
        else if (move_str == "none")
        {
            move = turing_tape_move::None;
        }
        else
        {
            std::stringstream stream;
            stream << "Unknown move '" << move_str << "'.";
            throw std::invalid_argument(stream.str());
        }

        transitions.push_back({{state, symbol_str[0]}, {new_state, new_symbol_str[0], move}});
    }

    return std::make_unique<turing_machine>(
        states.begin(),
        states.end(),
        initial_state,
        final_states.begin(),
        final_states.end(),
        symbols.begin(),
        symbols.end(),
        empty_symbol[0],
        transitions.begin(),
        transitions.end());
}
