mod machine;

fn main() {
   let mut machine = machine::io::load_turing_machine_from_toml_string("");
   machine.run();
   println!("{:#?}", machine);
}
