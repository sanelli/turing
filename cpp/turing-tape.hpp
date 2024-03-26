#pragma once

#include <vector>
#include <tuple>

#include "turing-typing.hpp"

namespace turing
{
    enum class turing_tape_move
    {
        None,
        Left,
        Right,
    };

    class turing_tape
    {
        int currentPosition;
        std::vector<turing_symbol> positivePositions;
        std::vector<turing_symbol> negativePositions;
        turing_symbol emtpySymbol;

    public:
        turing_tape(turing_symbol emtpySymbol);

        void set(turing_symbol symbol);
        turing_symbol get();

        void move(turing_tape_move m);

        void clear();
        template <typename TIterator>
        void initialize(TIterator first, TIterator last, bool resetPosition = true)
        {
            clear();
            for (auto iterator = first; iterator != last; iterator++)
            {
                set(*iterator);
                move(turing_tape_move::Right);
            }

            if (resetPosition)
            {
                currentPosition = 0;
            }
        }

        std::string str(turing_symbol separator = '|') const;

    private:
        int getIndexForCurrentPosition();
        void ensureTapeHasSpaceForPosition();
        std::vector<turing_symbol> &getPositionsVector();
    };
}