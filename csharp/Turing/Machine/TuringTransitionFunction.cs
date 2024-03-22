// <copyright file="TuringTransitionFunction.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine;

/// <summary>
/// Describe the transition function for a turing machine.
/// </summary>
public sealed class TuringTransitionFunction
{
    private readonly string haltStatus;
    private readonly Dictionary<(string State, char Symbol), (string State, char Symbol, TuringCursorMovement Move)> transitions = new();

    /// <summary>
    /// Initializes a new instance of the <see cref="TuringTransitionFunction"/> class.
    /// </summary>
    /// <param name="haltStatus">The halt status returned when no transition can be found.</param>
    public TuringTransitionFunction(string haltStatus)
    {
        this.haltStatus = haltStatus;
    }

    /// <summary>
    /// Add a new transition.
    /// </summary>
    /// <param name="from">The origin of the transition.</param>
    /// <param name="to">The target of the transition.</param>
    /// <exception cref="ArgumentException">When the origin of the transition has already been set.</exception>
    public void Set((string State, char Symbol) from, (string State, char Symbol, TuringCursorMovement Move) to)
    {
        if (!this.transitions.TryAdd(from, to))
        {
            throw new ArgumentException($"Transition origin ({from.State}, {from.Symbol}) already set", nameof(from));
        }
    }

    /// <summary>
    /// Get the target of a transition.
    /// If the transition do not exist the halt status is returned.
    /// </summary>
    /// <param name="state">The current state.</param>
    /// <param name="symbol">The symbol under the cursor.</param>
    /// <returns>The target of the transition.</returns>
    public (string State, char Symbol, TuringCursorMovement Move) Get(string state, char symbol)
    {
        if (this.transitions.TryGetValue((state, symbol), out var target))
        {
            return target;
        }

        return (this.haltStatus, symbol, TuringCursorMovement.None);
    }
}