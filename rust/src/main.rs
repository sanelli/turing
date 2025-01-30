use std::env;
use std::process::ExitCode;

mod machine;

fn main() -> ExitCode {
   let args: Vec<String> = env::args().collect();
   if args.len() != 4
   {
      eprintln!("Usage: {} <format> <input-file> <input-tape>", &args[0]);
      eprintln!("Formats:");
      eprintln!("  - toml: The TOML format of the file desribing the turing machine");
      return ExitCode::FAILURE;
   }

   let mut machine = machine::io::load_turing_machine_from_file(&args[1], &args[2]);
   machine.initialize(&args[3]);
   println!("Initial tape: |{}|", machine.tape_as_string());
   machine.run();
   println!("Final tape: |{}|", machine.tape_as_string());
   println!("Final state: '{}'", machine.current_state());

   return ExitCode::SUCCESS;
}
