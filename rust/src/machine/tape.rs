#[path = "./typing.rs"] mod typing;
use typing::{TuringSymbol};

#[derive(Debug, Default)]
pub enum TuringTapeMove
{
    #[default]
    None,
    Left,
    Right,
}

#[derive(Debug, Default)]
pub struct TuringTape
{
    current_position: i32,
    positive: Vec<TuringSymbol>,
    negative: Vec<TuringSymbol>,
}

impl TuringTape
{
    pub fn move_head(&mut self, direction: TuringTapeMove)
    {
        match direction {
            TuringTapeMove::None => { /* Nothing to do */},
            TuringTapeMove::Left => { self.current_position -= 1; },
            TuringTapeMove::Right => { self.current_position += 1; },
        };
    }

    pub fn current_position(&self) -> i32
    {
        self.current_position
    }

    pub fn get() -> TuringSymbol{
        todo!();
    }

    pub fn set(symbol: TuringSymbol) {
        todo!();
    }
}

#[test]
fn test_move_head()
{
    let mut tape = TuringTape::default();
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::None);
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::Right);
    assert_eq!(tape.current_position, 1);
    tape.move_head(TuringTapeMove::Left);
    assert_eq!(tape.current_position, 0);
}

#[test]
fn test_current_position()
{
    let mut tape = TuringTape::default();
    assert_eq!(tape.current_position(), 0);
    tape.move_head(TuringTapeMove::None);
    assert_eq!(tape.current_position(), 0);
    tape.move_head(TuringTapeMove::Right);
    assert_eq!(tape.current_position(), 1);
    tape.move_head(TuringTapeMove::Left);
    assert_eq!(tape.current_position(), 0);
}