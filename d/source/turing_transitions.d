module turing_transitions;

import turing_typing;

class TransitionFunctionFrom
{
    private TuringSymbol symbol;
    private TuringState state;

    this(TuringSymbol symbol, TuringState state)
    {
        this.symbol = symbol;
        this.state = state;
    }

    bool opEquals(const TransitionFunctionFrom other) const
    {
        return this.symbol == other.symbol &&
            this.state == other.state;
    }

    override size_t toHash() const
    {
        return this.symbol.hashOf() ^ this.state.hashOf();
    }

    override bool opEquals(Object o) const
    {
        auto other = cast(TransitionFunctionFrom) o;
        return this.opEquals(other);
    }
}

class TransitionFunctionTo
{
    private TuringSymbol symbol;
    private TuringState state;
    private TuringMoveDirection move;

    this(TuringSymbol symbol, TuringState state, TuringMoveDirection move) nothrow
    {
        this.symbol = symbol;
        this.state = state;
        this.move = move;
    }

    bool opEquals(const TransitionFunctionTo other) const
    {
        return this.symbol == other.symbol &&
            this.state == other.state &&
            this.move == other.move;
    }

    override size_t toHash() const
    {
        return this.symbol.hashOf() ^ this.state.hashOf() ^ (cast(int) this.move).hashOf();
    }

    override bool opEquals(Object o) const
    {
        auto other = cast(TransitionFunctionTo) o;
        return this.opEquals(other);
    }

    TuringSymbol getSymbol() const nothrow
    {
        return this.symbol;
    }

    TuringState getState() const nothrow
    {
        return this.state;
    }

    TuringMoveDirection getMove() const nothrow
    {
        return this.move;
    }
}

class TransitionFunction
{
    private TuringState haltState;
    private TransitionFunctionTo[TransitionFunctionFrom] transitions;

    bool add(TransitionFunctionFrom from, TransitionFunctionTo to)
    {
        TransitionFunctionTo* found = from in this.transitions;
        if (found == null)
        {
            this.transitions[from] = to;
            return true;
        }

        return false;
    }

    TransitionFunctionTo next(TransitionFunctionFrom from) const nothrow
    {
        auto found = from in this.transitions;
        if (found == null)
        {
            return new TransitionFunctionTo(from.symbol, this.haltState, TuringMoveDirection.None);
        }

        return cast(TransitionFunctionTo) *found;
    }
}
