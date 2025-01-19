mod tape;
use tape::TuringTape;

mod transition;
use transition::TuringTransitionFunction;

#[derive(Debug)]
pub struct TuringMachine
{
    tape: TuringTape,
    transition_function: TuringTransitionFunction,
}

impl TuringMachine {
    pub fn import_from_file() -> TuringMachine
    {
        TuringMachine { 
            tape: TuringTape::new(' '),
            transition_function: TuringTransitionFunction::new("".to_string()),
        }
    }
}