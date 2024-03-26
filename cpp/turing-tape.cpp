#include "turing-tape.hpp"

#include <vector>
#include <tuple>
#include <utility>
#include <stdexcept>
#include <sstream>

using namespace turing;

turing::turing_tape::turing_tape(turing_symbol emtpySymbol)
{
    this->emtpySymbol = emtpySymbol;

    currentPosition = 0;
    positivePositions.clear();
    negativePositions.clear();
}

void turing_tape::set(turing_symbol symbol)
{
    ensureTapeHasSpaceForPosition();
    const auto index = getIndexForCurrentPosition();
    auto &positions = getPositionsVector();
    positions[index] = symbol;
}

turing_symbol turing_tape::get()
{
    ensureTapeHasSpaceForPosition();
    const auto index = getIndexForCurrentPosition();
    auto &positions = getPositionsVector();
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

std::string turing_tape::str(turing_symbol separator) const
{
    std::stringstream output;

    // If the sape is not empty I need to add the separator at the beginning and at the end of the string
    if(negativePositions.size() <= 0 && negativePositions.size() <= 0)
    {
        return "";
    }

    // Head separator.
    output << separator;

    // Add negative tape reverted
    auto first = true;
    for(auto iterator = negativePositions.rbegin() ; iterator != negativePositions.rend() ; iterator++)
    {
        if(!first){
            output << separator;
        }

        output << *iterator;
        first = false;
    }

    // if first is negative then something has been added at least once 
    // and need to add se separator between negative and positive
    if(!first) 
    {
        output << separator;
    }

    // Add positive tape reverted
    first = true;
    for(auto iterator = positivePositions.begin() ; iterator != positivePositions.end() ; iterator++)
    {
        if(!first){
            output << separator;
        }

        output << *iterator;
        first = false;
    }

    // Final separator.
    output << separator;

    return  output.str();
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
    auto &positions = getPositionsVector();
    const auto index = getIndexForCurrentPosition();

    while (positions.size() <= static_cast<std::size_t>(index))
    {
        positions.push_back(emtpySymbol);
    }
}

std::vector<turing_symbol> &turing_tape::getPositionsVector()
{
    if (currentPosition >= 0)
    {
        return positivePositions;
    }
    return negativePositions;
}