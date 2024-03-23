// <copyright file="TuringTape.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using Turing.Extensions;

namespace Turing.Machine;

/// <summary>
/// Describe an "infinite" tape that can be used by the turing machine.
/// The tape is endless in both direction and allows storing
/// the symbol and moving left or right.
/// </summary>
public sealed class TuringTape
{
    private readonly char emptySymbol;
    private readonly List<char> negativePositions = new();
    private readonly List<char> positivePositions = new(); // Include position 0

    private int cursorPosition = 0;

    /// <summary>
    /// Initializes a new instance of the <see cref="TuringTape"/> class.
    /// </summary>
    /// <param name="emptySymbol">The character representing the empty symbol.</param>
    public TuringTape(char emptySymbol = ' ')
    {
        this.emptySymbol = emptySymbol;
    }

    /// <summary>
    /// Set the symbol at the current position.
    /// </summary>
    /// <param name="symbol">The symbol to set at the current position.</param>
    public void SetSymbol(char symbol)
    {
        var (tape, index) = this.GetIndexFromCurrentPosition();
        this.EnsureTapeHasSpaceForIndex(tape, index);
        tape[index] = symbol;
    }

    /// <summary>
    /// Get the symbol at the current position.
    /// </summary>
    /// <returns>The symbol at the current position.</returns>
    public char GetSymbol()
    {
        var (tape, index) = this.GetIndexFromCurrentPosition();
        this.EnsureTapeHasSpaceForIndex(tape, index);
        return tape[index];
    }

    /// <summary>
    /// Move the current position to the left.
    /// </summary>
    public void MoveLeft() => --this.cursorPosition;

    /// <summary>
    /// Move the current position to the right.
    /// </summary>
    public void MoveRight() => ++this.cursorPosition;

    /// <summary>
    /// Move the cursor according to the specified movement.
    /// </summary>
    /// <param name="movement">The movement.</param>
    /// <exception cref="ArgumentException">When <paramref name="movement"/> is invalid.</exception>
    public void Move(TuringCursorMovement movement)
    {
        switch (movement)
        {
            case TuringCursorMovement.Left:
                this.MoveLeft();
                break;
            case TuringCursorMovement.Right:
                this.MoveRight();
                break;
            case TuringCursorMovement.None:
                break;
            default:
                throw new ArgumentException("Invalid movement", nameof(movement));
        }
    }

    /// <summary>
    /// Clear the tape from it's content.
    /// </summary>
    public void Clear()
    {
        this.cursorPosition = 0;
        this.negativePositions.Clear();
        this.positivePositions.Clear();
    }

    /// <summary>
    /// Initialize the tape with a set of symbols and allows to return
    /// the cursor to the initial position.
    /// </summary>
    /// <param name="initialSymbols">The list of initial symbols to place on the tape.</param>
    /// <param name="resetPosition"><c></c> if the cursor should be placed at the "beginning" of the tape, <c>false</c> otherwise.</param>
    public void Initialize(IEnumerable<char> initialSymbols, bool resetPosition = true)
    {
        this.Clear();

        foreach (var symbol in initialSymbols)
        {
            this.SetSymbol(symbol);
            this.MoveRight();
        }

        if (resetPosition)
        {
            this.cursorPosition = 0;
        }
    }

    /// <summary>
    /// String representation the tape using the specified separator between cells.
    /// </summary>
    /// <param name="separator">The separator used to separate the cells.</param>
    /// <returns>The string representation of the tape.</returns>
    public string ToString(char separator)
    {
        var negative = string.Join(separator, this.negativePositions).Reverse().Trim(this.emptySymbol).Reverse();
        var positive = string.Join(separator, this.positivePositions).Trim(this.emptySymbol);
        var separatorBetweenPositiveAndNegative = negative.Length > 0 ? $"{separator}" : string.Empty;
        return negative + separatorBetweenPositiveAndNegative + positive;
    }

    /// <inheritdoc />
    public override string ToString()
        => this.ToString('|');

    private (List<char> PartialTape, int Index) GetIndexFromCurrentPosition()
    {
        return this.cursorPosition >= 0
            ? (this.positivePositions, this.cursorPosition)
            : (this.negativePositions, -this.cursorPosition - 1);
    }

    private void EnsureTapeHasSpaceForIndex(ICollection<char> partialTape, int index)
    {
        while (partialTape.Count <= index)
        {
            partialTape.Add(this.emptySymbol);
        }
    }
}