mod tape;
use tape::TuringTape;

#[derive(Debug)]
pub struct TuringMachine
{
    tape: TuringTape,
}

impl TuringMachine {
    pub fn import_from_file() -> TuringMachine
    {
        TuringMachine { 
            tape: TuringTape::new(' '),
        }
    }
}