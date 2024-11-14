import std.stdio;
import std.getopt;
import std.string;

import turing_machine_io;
import turing_machine;

int main(string[] args)
{
	try
	{
		string format, filename, input;
		auto arguments = getopt(args,
			std.getopt.config.required, "format|m", "The format of the program (supporter: toml).", &format,
			std.getopt.config.required, "filename|f", "The file containing the turing machine description.", &filename,
			std.getopt.config.required, "input|i", "The initial content of the tape.", &input);

		if (arguments.helpWanted)
		{
			defaultGetoptPrinter("Execute the turing machine interpreter.", arguments.options);
			return 0;
		}

		auto machine = loadTuringMachineFromFile(format, filename);
		if(machine is null)
		{
			writeln("ERROR: File does not exists or format is incorrect");
			"Usage: %s --format <format> --filename <filename> --input <input>".format(args[0]).writeln;
			return 1;
		}

		machine.clear(input);

		"Initial tape: |%s|".format(machine.getTape()).writeln;
		machine.run();
		"Final tape: |%s|".format(machine.getTape()).writeln;
		"Final state: '%s'".format(machine.getCurrentState()).writeln;

		return 0;
	}
	catch (std.getopt.GetOptException e)
	{
		"Usage: %s --format <format> --filename <filename> --input <input>".format(args[0]).writeln;
		return 1;
	}
}
