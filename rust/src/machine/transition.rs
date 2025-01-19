#[path = "./typing.rs"]
mod typing;
use typing::{TuringSymbol, TuringStatus};

#[path = "./tape.rs"]
mod tape;
use tape::TuringTapeMove;

use std::collections::HashMap;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct TuringTransitionFunctionFrom {
    status: TuringStatus,
    symbol: TuringSymbol,
}

impl TuringTransitionFunctionFrom
{
    pub fn new(status: TuringStatus, symbol: TuringSymbol) -> Self {
        Self {
            status: status.clone(),
            symbol
        }
    }

    pub fn status(&self) -> TuringStatus
    {
        (&self.status).clone()
    }

    pub fn symbol(&self) -> TuringSymbol
    {
        self.symbol
    }
}

#[derive(Debug, Clone)]
pub struct TuringTransitionFunctionTo {
    status: TuringStatus,
    symbol: TuringSymbol,
    move_direction: TuringTapeMove,
}

impl TuringTransitionFunctionTo
{
    pub fn new(status: TuringStatus, symbol: TuringSymbol, move_direction: TuringTapeMove) -> Self {
        Self {
            status: status.clone(),
            symbol: symbol,
            move_direction: move_direction.clone(),
        }
    }

    pub fn status(&self) -> TuringStatus
    {
        (&self.status).clone()
    }

    pub fn symbol(&self) -> TuringSymbol
    {
        self.symbol
    }

    pub fn move_direction(&self) -> TuringTapeMove
    {
        self.move_direction.clone()
    }
}

#[derive(Debug)]
pub struct TuringTransitionFunction 
{
    transitions: HashMap<TuringTransitionFunctionFrom, TuringTransitionFunctionTo>,
    halt_status: TuringStatus,
}

impl TuringTransitionFunction {
    pub fn new(halt_status: TuringStatus) -> Self
    {
        Self
        {
            transitions: HashMap::new(),
            halt_status,
        }
    }

    pub fn add_transition(&mut self, from: TuringTransitionFunctionFrom, to: TuringTransitionFunctionTo)
    {
        self.transitions.insert(from, to);
    }

    pub fn get_transaction(&self, from: &TuringTransitionFunctionFrom) -> TuringTransitionFunctionTo
    {
        if let Some(to)= self.transitions.get(from) { 
           to.clone()
        } else 
        {
            TuringTransitionFunctionTo::new(self.halt_status.clone(), from.symbol,TuringTapeMove::None)
        }
    }
}
