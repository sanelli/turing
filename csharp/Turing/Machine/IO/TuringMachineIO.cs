// <copyright file="TuringMachineIO.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine.IO;

/// <summary>
/// Read the machine definition from a file.
/// </summary>
public static class TuringMachineIO
{
    /// <summary>
    /// Read a file from TOML file.
    /// </summary>
    /// <param name="textFileContent">The content of the file.</param>
    /// <returns>A turing machine setup to run the instructions described by the file.</returns>
    /// <seealso href="https://toml.io/en/"/>
    public static TuringMachine FromTomlText(string textFileContent)
    {
        var machineDescription = Tomlet.TomletMain.To<TuringMachineToml>(textFileContent);
        return new TuringMachine(
            machineDescription.States,
            machineDescription.InitialState,
            machineDescription.FinalStates,
            machineDescription.Symbols.Select(GetSymbolFromString).ToArray(),
            GetSymbolFromString(machineDescription.EmptySymbol),
            machineDescription.Transitions.Select(AsTransitionTuple).ToArray());
    }

    private static ((string State, char Symbol) From, (string State, char Symbol, TuringCursorMovement Move) To) AsTransitionTuple(TuringMachineTransitionToml transition)
        => ((transition.State, GetSymbolFromString(transition.Symbol)), (transition.NewState, GetSymbolFromString(transition.NewSymbol), GetMovementFromString(transition.Move)));

    private static char GetSymbolFromString(string value)
    {
        if (value.Length == 1)
        {
            return value[0];
        }

        throw new ArgumentException($"'{value}' is not a valid symbol");
    }

    private static TuringCursorMovement GetMovementFromString(string movement)
    {
        return new[] { TuringCursorMovement.Left, TuringCursorMovement.Right, TuringCursorMovement.None }
            .Single(enumValue => enumValue.ToString().Equals(movement, StringComparison.InvariantCultureIgnoreCase));
    }
}