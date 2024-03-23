// <copyright file="TuringMachineTransitionToml.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine.IO;

/// <summary>
/// Describe an entry in the transition table
/// for a TOML file.
/// </summary>
/// <param name="State">The start state of the transition.</param>
/// <param name="Symbol">The start symbol of the transition.</param>
/// <param name="NewState">The new state to be applied.</param>
/// <param name="NewSymbol">The symbol to be stored on the tape at the current location.</param>
/// <param name="Move">The move to be applied to the cursor (left, right, none).</param>
public sealed record TuringMachineTransitionToml(
    string State,
    string Symbol,
    string NewState,
    string NewSymbol,
    string Move);