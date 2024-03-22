// <copyright file="StringExtensions.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

namespace Turing.Extensions;

/// <summary>
/// Extensions methods for <see cref="string"/>.
/// </summary>
public static class StringExtensions
{
    /// <summary>
    /// Return the string reversed.
    /// </summary>
    /// <param name="s">The inout string.</param>
    /// <returns>The reversed string.</returns>
    public static string Reverse(this string s)
    {
        var array = s.ToCharArray();
        Array.Reverse(array);
        return new string(array);
    }
}