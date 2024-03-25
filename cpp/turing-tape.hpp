#pragma once

#include <vector>
#include <tuple>

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
        std::vector<char> positivePositions;
        std::vector<char> negativePositions;
        char emtpySymbol;

    public:
        turing_tape(char emtpySymbol);

        void set(char symbol);
        char get();

        void move(turing_tape_move m);

        void clear();
        template <typename TIteratorBegin, typename TIteratorEnd>
        void initialize(TIteratorBegin first, TIteratorEnd last, bool resetPosition = true)
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

        std::string str(char separator = '|') const;

    private:
        int getIndexForCurrentPosition();
        void ensureTapeHasSpaceForPosition();
        std::vector<char> &getPositionsVector();
    };
}