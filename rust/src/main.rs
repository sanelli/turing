mod machine;

fn main() {
   let machine = machine::TuringMachine::import_from_file();
   println!("{:#?}", machine);
}
