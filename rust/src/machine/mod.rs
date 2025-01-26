pub mod io;
mod tape;
mod transition;
mod typing;

use std::collections::HashSet;
use tape::TuringTape;
use transition::TuringTransitionFunction;
use typing::TuringStatus;

#[derive(Debug)]
pub struct TuringMachine {
    tape: TuringTape,
    transition_function: TuringTransitionFunction,
    halt_statuses: HashSet<TuringStatus>,
    current_status: String,
}

impl TuringMachine {
    pub fn halted(&self) -> bool {
        self.halt_statuses.contains(&self.current_status)
    }

    pub fn step(&mut self) -> bool {
        if self.halted() {
            return false;
        }

        let current_symbol = self.tape.get_symbol();
        if let Some(to) = self
            .transition_function
            .next(&self.current_status, &current_symbol)
        {
            self.current_status = to.status().clone();
            self.tape.set_symbol(to.symbol());
            let move_direction = to.move_direction();
            self.tape.move_head(move_direction);
            true
        } else {
            self.current_status = self.halt_statuses.iter().nth(0).unwrap().clone();
            false
        }
    }

    pub fn run(&mut self) {
        while self.step() { /* Nothing to do */ }
    }
}
