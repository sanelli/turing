module turing_tape;

import std;
import optional;
import turing_typing;

class TuringTape
{
    private TuringSymbol[] negative;
    private TuringSymbol[] positive;
    private int currentPosition;
    private TuringSymbol emptySymbol;

    this(TuringSymbol emptySymbol) nothrow @safe
    {
        this.emptySymbol = emptySymbol;
        this.currentPosition = 0;
        this.positive = new TuringSymbol[0];
        this.negative = new TuringSymbol[0];
    }

    ~this()
    {
        this.currentPosition = 0;
        this.positive.length = 0;
        this.negative.length = 0;
    }

    void clear(string initialValue = "")
    {
        this.currentPosition = 0;
        this.positive.length = 0;
        this.negative.length = 0;

        foreach(char c; initialValue)
        {
            this.set(c);
            this.move(TuringMoveDirection.Right);
        }

        this.currentPosition = 0;
    }

    private void ensureTapeLength(int position) nothrow @safe
    {
        if (position >= 0 && position >= this.positive.length)
        {
            auto currentLength = this.positive.length;
            this.positive.length = position + 1;
            for (auto index = currentLength; index < this.positive.length; ++index)
            {
                this.positive[index] = this.emptySymbol;
            }
        }
        else if (position < 0 && (-position) > this.positive.length)
        {
            auto currentLength = this.negative.length;
            this.negative.length = -position;
            for (auto index = currentLength; index < this.negative.length; ++index)
            {
                this.negative[index] = this.emptySymbol;
            }
        }
    }

    void move(TuringMoveDirection direction) nothrow @safe
    {
        final switch (direction)
        {
        case TuringMoveDirection.Left:
            --this.currentPosition;
            break;
        case TuringMoveDirection.Right:
            ++this.currentPosition;
            break;
        case TuringMoveDirection.None:
            // Nothing to do;
            break;
        }
    }

    void set(TuringSymbol symbol) nothrow @safe
    {
        ensureTapeLength(this.currentPosition);
        if(this.currentPosition >= 0)
        {
            this.positive[this.currentPosition] = symbol;
        } else {
            this.positive[-this.currentPosition - 1] = symbol;
        }
    }

    TuringSymbol get() nothrow @safe
    {
        ensureTapeLength(this.currentPosition);
        if(this.currentPosition >= 0)
        {
            return this.positive[this.currentPosition];
        } else {
            return this.positive[-this.currentPosition - 1];
        }
    }

    string toString(char separator) const nothrow @safe
    {
        auto result = "";
        result ~= separator;

        for (auto index = (cast(long)this.negative.length) - 1; index >= 0; --index)
        {
            result ~= this.negative[index];
            result ~= separator;
        }

        for (auto index = 0; index < this.positive.length; ++index)
        {
            result ~= this.positive[index];
            result ~= separator;
        }

        if(result.length == 1)
        {
                result ~= separator;    
        }

        return result;
    }
}
