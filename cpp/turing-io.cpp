#include <memory>
#include <string>
#include <sstream>
#include <stdexcept>

#include <toml.hpp>

#include "turing-machine.hpp"
#include "turing-io.hpp"

using namespace turing;

std::unique_ptr<turing::turing_machine> turing::make_turing_machine(const std::string& format, const std::string& machine)
{
    if(format == "toml")
    {
        return make_turing_machine_from_toml(machine);
    }

    throw std::invalid_argument("Unknown format");
}

std::unique_ptr<turing::turing_machine> turing::make_turing_machine_from_toml(const std::string& machine)
{
    std::istringstream iss{machine};
    const auto data = toml::parse(iss);

    std::set<turing_state> states;

    return std::make_unique<turing_machine>(states.begin(), states.end(), "");
}
