// <copyright file="TuringMachine.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine;

/// <summary>
/// Describe a turing machine interpreter.
/// </summary>
public sealed class TuringMachine
{
    private readonly string initialState;
    private readonly HashSet<string> finalStates;
    private readonly HashSet<char> symbols;

    private readonly TuringTape tape;
    private readonly TuringTransitionFunction transitions;

    private string currentState;

    /// <summary>
    /// Initializes a new instance of the <see cref="TuringMachine"/> class.
    /// </summary>
    /// <param name="states">The set of states.</param>
    /// <param name="initialState">The initial state.</param>
    /// <param name="finalStates">The set of final states.</param>
    /// <param name="symbols">The set of symbols.</param>
    /// <param name="emptySymbol">The empty symbol.</param>
    /// <param name="transactions">The set of transactions.</param>
    public TuringMachine(
        IEnumerable<string> states,
        string initialState,
        IEnumerable<string> finalStates,
        IEnumerable<char> symbols,
        char emptySymbol,
        IEnumerable<((string State, char Symbol) From, (string State, char Symbol, TuringCursorMovement Move) To)> transactions)
    {
        this.initialState = initialState;
        this.currentState = this.initialState;
        this.symbols = [..symbols];
        this.finalStates = [..finalStates];

        HashSet<string> uniqueStates = [..states];

        var anyInvalidState = uniqueStates.FirstOrDefault(string.IsNullOrWhiteSpace);
        if (anyInvalidState is not null)
        {
            throw new ArgumentException($"Invalid state '{anyInvalidState}': it cannot be null or white space.", nameof(states));
        }

        if (string.IsNullOrWhiteSpace(this.initialState))
        {
            throw new ArgumentException($"Invalid initial state '{this.initialState}': it cannot be null or white space.", nameof(initialState));
        }

        if (!uniqueStates.Contains(this.initialState))
        {
            throw new ArgumentException($"Invalid initial state '{this.initialState}': it is not a state", nameof(initialState));
        }

        foreach (var finalState in this.finalStates)
        {
            if (string.IsNullOrWhiteSpace(finalState))
            {
                throw new ArgumentException($"Invalid final state '{finalState}': it cannot be null or white space.", nameof(finalStates));
            }

            if (!uniqueStates.Contains(finalState))
            {
                throw new ArgumentException($"Invalid final state '{finalState}': it not a state.", nameof(finalStates));
            }
        }

        if (this.finalStates.Count < 1)
        {
            throw new ArgumentException("At least one final state must be provided.", nameof(finalStates));
        }

        if (!this.symbols.Contains(emptySymbol))
        {
            throw new ArgumentException($"Empty symbol '{emptySymbol}' is not a valid symbol.", nameof(emptySymbol));
        }

        this.tape = new(emptySymbol);

        this.transitions = new(this.finalStates.First());
        foreach (var transaction in transactions)
        {
            if (string.IsNullOrWhiteSpace(transaction.From.State))
            {
                throw new ArgumentException($"Invalid from transaction state '{transaction.From.State}': it cannot be null or white space.", nameof(transactions));
            }

            if (!uniqueStates.Contains(transaction.From.State))
            {
                throw new ArgumentException($"Invalid from transaction state '{transaction.From.State}': it is not a state.", nameof(transactions));
            }

            if (!this.symbols.Contains(transaction.From.Symbol))
            {
                throw new ArgumentException($"Invalid from transaction symbol '{transaction.From.Symbol}': it is not a valid symbol.", nameof(transactions));
            }

            if (string.IsNullOrWhiteSpace(transaction.To.State))
            {
                throw new ArgumentException($"Invalid to transaction state '{transaction.To.State}': it cannot be null or white space.", nameof(transactions));
            }

            if (!uniqueStates.Contains(transaction.To.State))
            {
                throw new ArgumentException($"Invalid to transaction state '{transaction.To.State}': it is not a state.", nameof(transactions));
            }

            if (!this.symbols.Contains(transaction.To.Symbol))
            {
                throw new ArgumentException($"Invalid to transaction symbol '{transaction.To.Symbol}': it is not a valid symbol.", nameof(transactions));
            }

            this.transitions.Set(transaction.From, transaction.To);
        }
    }

    /// <summary>
    /// Gets the current state.
    /// </summary>
    public string CurrentState => this.currentState;

    /// <summary>
    /// Gets the current symbol.
    /// </summary>
    public char CurrentSymbol => this.tape.GetSymbol();

    /// <summary>
    /// Gets the current tape view.
    /// </summary>
    public string Tape => this.tape.ToString();

    /// <summary>
    /// Initialize the tape and reset the status to the initial status of the machine.
    /// </summary>
    /// <param name="initialTape">The symbols to be placed on the tape.</param>
    public void Clear(ICollection<char> initialTape)
    {
        this.currentState = this.initialState;
        foreach (var symbol in initialTape)
        {
            if (!this.symbols.Contains(symbol))
            {
                throw new ArgumentException($"Invalid symbol '{symbol}': it is not a supported symbol.", nameof(initialTape));
            }
        }

        this.tape.Initialize(initialTape);
    }

    /// <summary>
    /// Check if the machine has terminated on an halt state.
    /// </summary>
    /// <returns><c>true</c> if the machine reached an halt state, <c>false</c> otherwise.</returns>
    public bool HasHalted() => this.finalStates.Contains(this.currentState);

    /// <summary>
    /// Perform a step in the computation.
    /// </summary>
    public void Step()
    {
        if (this.HasHalted())
        {
            return;
        }

        var (nextState, symbol, move) = this.transitions.Get(this.currentState, this.tape.GetSymbol());
        this.currentState = nextState;
        this.tape.SetSymbol(symbol);
        this.tape.Move(move);
    }

    /// <summary>
    /// Run until the machine reaches an halt state.
    /// </summary>
    public void Run()
    {
        while (!this.HasHalted())
        {
            this.Step();
        }
    }
}