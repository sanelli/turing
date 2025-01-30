use crate::machine::tape::TuringTapeMove::*;
use crate::machine::tape::{TuringTape, TuringTapeMove};
use crate::machine::transition::{
    TuringTransitionFunction, TuringTransitionFunctionFrom, TuringTransitionFunctionTo,
};
use crate::machine::TuringMachine;
use serde::Deserialize;
use std::collections::HashSet;
use toml;
use std::fs;

#[derive(Deserialize)]
struct TuringMachineTransitionIo {
    #[serde(rename = "State")]
    state: String,
    #[serde(rename = "Symbol")]
    symbol: String,
    #[serde(rename = "NewState")]
    new_state: String,
    #[serde(rename = "NewSymbol")]
    new_symbol: String,
    #[serde(rename = "Move")]
    move_direction: String,
}

#[derive(Deserialize)]
struct TuringMachineIo {
    #[serde(rename = "States")]
    states: Vec<String>,
    #[serde(rename = "InitialState")]
    initial_state: String,
    #[serde(rename = "FinalStates")]
    final_states: Vec<String>,
    #[serde(rename = "Symbols")]
    symbols: Vec<String>,
    #[serde(rename = "EmptySymbol")]
    empty_symbol: String,
    #[serde(rename = "Transitions")]
    transitions: Vec<TuringMachineTransitionIo>,
}

pub fn load_turing_machine_from_file<T>(format: T, filename: T) -> TuringMachine
where
T: AsRef<str>,
{
    return match format.as_ref() 
    {
        "toml" => load_turing_machine_from_toml_file(filename),
        _ => panic!("Unexpected format"),
    } ;
}

fn load_turing_machine_from_toml_file<T>(filename: T) -> TuringMachine
where
    T: AsRef<str>,
{
    let toml = match fs::read_to_string(filename.as_ref()) 
    {
        Ok(content) => content,
        Err(e) => panic!("Cannot find file: {}", e),
    };

    load_turing_machine_from_toml_string(toml)
}

fn load_turing_machine_from_toml_string<T>(input: T) -> TuringMachine
where
    T: AsRef<str>,
{
    let str_ref = input.as_ref();
    let data: TuringMachineIo = match toml::from_str(str_ref) {
        Ok(d) => d,
        Err(e) => panic!("Cannot parse toml string: {}", e),
    };

    // Check states
    if data.states.len() < 1 {
        panic!("Not enought states");
    }

    for state in data.states.iter() {
        if state.len() < 1 {
            panic!("State lenghts is invalid");
        }
    }

    // Handle symbols
    for symbol in data.symbols.iter() {
        if symbol.len() != 1 {
            panic!("Symbols lenghts must be exactly one character");
        }
    }

    // Handle empty symbol
    if data.empty_symbol.len() != 1 {
        panic!("The empty symbol must be exactly one character long");
    }

    if !data.symbols.contains(&data.empty_symbol) {
        panic!("The empty symbol is not a valid symbol");
    }

    let empty_symbol;
    if let Some(some_empty_symbol) = data.empty_symbol.chars().nth(0) {
        empty_symbol = some_empty_symbol;
    } else {
        panic!("Invalid empty symbol");
    }

    // Handle halt statuses
    if data.final_states.len() <= 0 {
        panic!("No final status has been provided");
    }
    for halt_status in data.final_states.iter() {
        if !data.states.contains(&halt_status) {
            panic!("The list of halt statuses is missing");
        }
    }

    let halt_statuses: HashSet<String> = data.final_states.into_iter().collect();

    // Handle initial state
    if !data.states.contains(&data.initial_state) {
        panic!("Invalid initial state");
    }

    // Transactions
    if data.transitions.len() <= 0 {
        panic!("At least one transaction must be provided");
    }

    let mut transitions = TuringTransitionFunction::new();
    for transaction_io in data.transitions.iter() {
        // Chheck from state
        if !data.states.contains(&transaction_io.state) {
            panic!("Transaction contains an invalid from state");
        }
        let status = transaction_io.state.clone();

        // Check from symbol
        if !data.symbols.contains(&transaction_io.symbol) {
            panic!("Transaction contains an invalid from symbol");
        }
        let symbol = transaction_io.symbol.chars().nth(0).unwrap();

        // Check to state
        if !data.states.contains(&transaction_io.new_state) {
            panic!("Transaction contains an invalid to state");
        }
        let to_status = transaction_io.new_state.clone();

        // Check to symbol
        if !data.symbols.contains(&transaction_io.new_symbol) {
            panic!("Transaction contains an invalid to symbol");
        }
        let to_symbol = transaction_io.new_symbol.chars().nth(0).unwrap();

        // Check move
        let move_direction: TuringTapeMove;
        match transaction_io.move_direction.as_str() {
            "left" => move_direction = Left,
            "right" => move_direction = Right,
            "none" => move_direction = None,
            _ => panic!("Transaction contains an invalid"),
        }

        // Generate from and to
        let from = TuringTransitionFunctionFrom::new(status, symbol);
        let to = TuringTransitionFunctionTo::new(to_status, to_symbol, move_direction);
        transitions.add(from, to);
    }

    TuringMachine {
        tape: TuringTape::new(empty_symbol),
        transition_function: transitions,
        halt_statuses: halt_statuses,
        current_status: data.initial_state,
    }
}

#[test]
fn test_load_turing_machine_from_toml_string() {
    let input = r#"
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
    "#;
    let mut machine = load_turing_machine_from_toml_string(input);
    machine.initialize(&"abba".to_owned());
    machine.run();
    assert!(machine.halted());
    assert_eq!(machine.current_status, "halt");
    assert_eq!(machine.tape.to_string(), "|b|a|a|b| |")
}
