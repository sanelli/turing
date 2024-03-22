// <copyright file="TuringCursorMovement.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Machine;

/// <summary>
/// Describe the movement of the cursor.
/// </summary>
public enum TuringCursorMovement
{
    /// <summary>
    /// The cursor should not move.
    /// </summary>
    None,

    /// <summary>
    /// The cursor should move to the lest.
    /// </summary>
    Left,

    /// <summary>
    /// The cursor should move to the right.
    /// </summary>
    Right,
}