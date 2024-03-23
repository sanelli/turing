// <copyright file="TuringMachineToml.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine.IO;

/// <summary>
/// Describe a turing machine setup as stored in a TOML file.
/// </summary>
/// <param name="States">The list of states.</param>
/// <param name="InitialState">The initial state.</param>
/// <param name="FinalStates">The final states.</param>
/// <param name="Symbols">The list of symbols.</param>
/// <param name="EmptySymbol">The symbol representing an empty value.</param>
/// <param name="Transitions">The transitions.</param>
public record TuringMachineToml(
    string[] States,
    string InitialState,
    string[] FinalStates,
    string[] Symbols,
    string EmptySymbol,
    TuringMachineTransitionToml[] Transitions);