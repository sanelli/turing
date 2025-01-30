use std::collections::HashMap;
use crate::machine::typing::{TuringSymbol, TuringStatus};
use crate::machine::tape::TuringTapeMove;

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
}

impl TuringTransitionFunction {
    pub fn new() -> Self
    {
        Self
        {
            transitions: HashMap::new(),
        }
    }

    pub fn add(&mut self, from: TuringTransitionFunctionFrom, to: TuringTransitionFunctionTo)
    {
        self.transitions.insert(from, to);
    }

    pub fn next(&self, status: &TuringStatus, symbol: &TuringSymbol) -> Option<&TuringTransitionFunctionTo>
    {
        if let Some(to)= self.transitions.get(&TuringTransitionFunctionFrom{ 
            status: status.to_string(),
            symbol: symbol.clone() }) { 
           Some(&to)
        } else 
        {
            None
        }
    }
}
