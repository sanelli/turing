#pragma once
#include <memory>
#include <string>

#include "turing-machine.hpp"

namespace turing
{
    std::unique_ptr<turing_machine> make_turing_machine(const std::string& format, const std::string& machine);
    std::unique_ptr<turing_machine> make_turing_machine_from_toml(const std::string& machine);
}