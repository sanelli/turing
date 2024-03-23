// <copyright file="Program.cs" company="Stefano Anelli">
// Copyright (c) Stefano Anelli. All rights reserved.
// </copyright>

using Turing.Machine;
using Turing.Machine.IO;

if (args.Length < 3)
{
    Console.Error.WriteLine("Usage: ./Turing \"file-type\" \"filename\" \"initial-tape\"");
    Console.Error.WriteLine("Supported types:");
    Console.Error.WriteLine("   - toml: TOML format");
    Environment.Exit(1);
}

var format = args[0];
var filename = args[1];
var initialTape = args[2];

TuringMachine machine = format switch
{
    "toml" => TuringMachineIO.FromTomlText(File.ReadAllText(filename)),
    _ => throw new ArgumentException("unknown format"),
};

machine.Clear(initialTape.ToCharArray());
Console.WriteLine($"Initial tape: |{machine.Tape}|");

machine.Run();
Console.WriteLine($"Final tape: |{machine.Tape}|");
Console.WriteLine($"Final state: '{machine.CurrentState}'");

Environment.Exit(0);