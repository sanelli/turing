module turing_machine;

import turing_typing;
import turing_tape;
import turing_transitions;

class TuringMachine
{
    private TuringTape tape;
    private TransitionFunction transitions;
    private bool[TuringState] haltStates;
    private TuringState currentState;
    private TuringState initialState;

    this(
        TuringSymbol emptySymbol,
        TuringState initialState,
        TuringState[] haltStates,
        TransitionFunction transitions)
    {
        this.tape = new TuringTape(emptySymbol);
        this.transitions = transitions;
        this.initialState = initialState;
        this.currentState = initialState;
        foreach(TuringState haltState; haltStates)
        {
            this.haltStates[haltState] = true;
        }
    }

    void clear(string initialTape = "")
    {
        this.currentState = this.initialState;
        this.tape.clear(initialTape);
    }

    bool halted() const
    {
        auto haltState = this.currentState in this.haltStates;
        return haltState !is null;
    }

    bool step()
    {
        if (this.halted())
        {
            return false;
        }

        auto currentSymbol = this.tape.get();
        auto from = new TransitionFunctionFrom(currentSymbol, this.currentState);
        auto to = this.transitions.next(from);

        this.currentState = to.getState();
        this.tape.set(to.getSymbol());
        this.tape.move(to.getMove());

        return true;
    }

    void run()
    {
        while (this.step())
        {
        }
    }

    string getTape(char separator = '|')
    {
        return this.tape.toString(separator);
    }

    TuringState getCurrentState() const
    {
        return this.currentState;
    }
}
