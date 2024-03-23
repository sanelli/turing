// <copyright file="TuringMachineTests.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using FluentAssertions;

using Turing.Machine.IO;

using Xunit.Categories;

namespace Turing.Tests;

/// <summary>
/// Tests for the turing machine.
/// </summary>
public sealed class TuringMachineTests
{
    /// <summary>
    /// Gets the input data for method <see cref="TestTuringMachine"/>.
    /// </summary>
    /// <returns>The input data for method <see cref="TestTuringMachine"/>.</returns>
    public static IEnumerable<object[]> TestTuringMachineInput()
    {
        yield return
        [
            """
            # Replace all a characters with b and vice-versa.
            # Replace a with b and with a.

            States = [ "replace", "halt" ]
            InitialState = "replace"
            FinalStates = [ "halt" ]
            Symbols = [ " ", "a", "b" ]
            EmptySymbol = " "

            [[Transitions]]
            State = "replace"
            Symbol = "a"
            NewState = "replace"
            NewSymbol = "b"
            Move = "right"

            [[Transitions]]
            State = "replace"
            Symbol = "b"
            NewState = "replace"
            NewSymbol = "a"
            Move = "right"

            [[Transitions]]
            State = "replace"
            Symbol = " "
            NewState = "halt"
            NewSymbol = " "
            Move = "right"
            """,
            "abba",
            "|b|a|a|b|",
        ];
    }

    /// <summary>
    /// Test the turing machine using different inputs.
    /// </summary>
    /// <param name="machineDescription">The TOML description of the machine.</param>
    /// <param name="inputTape">The input tape.</param>
    /// <param name="expectedOutputTape">The output tape.</param>
    [Theory]
    [UnitTest]
    [MemberData(nameof(TestTuringMachineInput))]
    public void TestTuringMachine(string machineDescription, string inputTape, string expectedOutputTape)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(machineDescription);
        ArgumentException.ThrowIfNullOrWhiteSpace(inputTape);
        ArgumentException.ThrowIfNullOrWhiteSpace(expectedOutputTape);

        // Arrange
        var machine = TuringMachineIO.FromTomlText(machineDescription);
        machine.Clear(inputTape.ToCharArray());

        // Act
        machine.Run();

        // Assert
        machine.Tape.Should().Be(expectedOutputTape);
    }
}