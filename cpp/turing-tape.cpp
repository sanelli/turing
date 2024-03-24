#include "turing-tape.hpp"

#include <vector>
#include <tuple>
#include <utility>
#include <stdexcept>

using namespace turing_machine;

turing_machine::turing_tape::turing_tape(char emtpySymbol)
{
    this->emtpySymbol = emtpySymbol;

    currentPosition = 0;
    positivePositions.clear();
    negativePositions.clear();
}

void turing_tape::set(char symbol)
{
    ensureTapeHasSpaceForPosition();
    auto index = getIndexForCurrentPosition();
    std::vector<char> &positions = getPositionsVector();
    positions[index] = symbol;
}

char turing_tape::get()
{
    ensureTapeHasSpaceForPosition();
    auto index = getIndexForCurrentPosition();
    std::vector<char> &positions = getPositionsVector();
    return positions[index];
}

void turing_tape::move(turing_tape_move m)
{
    switch (m)
    {
    case turing_tape_move::Left:
        --currentPosition;
        break;

    case turing_tape_move::Right:
        ++currentPosition;
        break;

    case turing_tape_move::None:
        break;

    default:
        throw std::invalid_argument("Unknown move");
        break;
    }
}

void turing_tape::clear()
{
    positivePositions.clear();
    negativePositions.clear();
    currentPosition = 0;
}

int turing_tape::getIndexForCurrentPosition()
{
    if (currentPosition >= 0)
    {
        return currentPosition;
    }

    return -currentPosition + 1;
}

void turing_tape::ensureTapeHasSpaceForPosition()
{
    std::vector<char> &positions = getPositionsVector();
    auto index = getIndexForCurrentPosition();

    while (positions.size() <= static_cast<std::size_t>(index))
    {
        positions.push_back(emtpySymbol);
    }
}

std::vector<char> &turing_tape::getPositionsVector()
{
    if (currentPosition >= 0)
    {
        return this->positivePositions;
    }
    return this->negativePositions;
}
