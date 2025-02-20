use crate::machine::typing::TuringSymbol;

#[derive(Debug, Default, Clone)]
pub enum TuringTapeMove {
    #[default]
    None,
    Left,
    Right,
}

#[derive(Debug)]
pub struct TuringTape {
    current_position: i32,
    positive: Vec<TuringSymbol>,
    negative: Vec<TuringSymbol>,
    empty_symbol: TuringSymbol,
}

impl TuringTape {
    pub fn new(empty_symbol: TuringSymbol) -> TuringTape {
        TuringTape {
            current_position: 0,
            positive: vec![],
            negative: vec![],
            empty_symbol,
        }
    }

    pub fn reset<T>(&mut self, symbols: T)
    where
        T: AsRef<str>
    {
        self.positive.clear();
        self.negative.clear();
        self.current_position = 0;
        for symbol in symbols.as_ref().chars()
        {
            self.set_symbol(symbol);
            self.move_head(TuringTapeMove::Right);
        }

        self.current_position = 0;
    }

    pub fn move_head(&mut self, direction: TuringTapeMove) {
        match direction {
            TuringTapeMove::None => { /* Nothing to do */ }
            TuringTapeMove::Left => {
                self.current_position -= 1;
            }
            TuringTapeMove::Right => {
                self.current_position += 1;
            }
        };
    }

    pub fn get_symbol(&mut self) -> TuringSymbol {
        self.ensure_tape_sizes();
        let (side, position) = self.get_tape_and_position_for_accessing();
        side[position]
    }

    pub fn set_symbol(&mut self, symbol: TuringSymbol) -> () {
        self.ensure_tape_sizes();
        let (side, position) = self.get_tape_and_position_for_accessing();
        side[position] = symbol;
    }

    fn ensure_tape_sizes(&mut self) {
        let ensure: usize;
        let side: &mut Vec<TuringSymbol>;

        if self.current_position >= 0 {
            ensure = (self.current_position + 1) as usize;
            side = &mut self.positive;
        } else {
            ensure = (-self.current_position) as usize;
            side = &mut self.negative;
        }

        Self::ensure_tape_size_for_specific_tape(side, ensure, self.empty_symbol);
    }

    fn get_tape_and_position_for_accessing(&mut self) -> (&mut Vec<TuringSymbol>, usize) {
        let position;
        let side;
        if self.current_position >= 0 {
            position = self.current_position as usize;
            side = &mut self.positive;
        } else {
            position = (-self.current_position + 1) as usize;
            side = &mut self.negative;
        }

        (side, position)
    }

    fn ensure_tape_size_for_specific_tape(
        side: &mut Vec<TuringSymbol>,
        ensure: usize,
        empty_symbol: TuringSymbol,
    ) {
        while side.len() < ensure {
            side.push(empty_symbol);
        }
    }
}

impl ToString for TuringTape {
    fn to_string(&self) -> String {
        let mut result = String::with_capacity((self.negative.len() + self.positive.len()) * 2 + 1);
        result += "|";

        for &symbol in &self.negative
        {
            result.push(symbol);
            result +=  "|";
        }

        for &symbol in &self.positive
        {
            result.push(symbol);
            result +=  "|";
        }

        if result.len() == 1
        {
            result += "|";
        }

        result
    }
}

#[test]
fn test_move_head() {
    let mut tape = TuringTape::new(' ');
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::None);
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::Right);
    assert_eq!(tape.current_position, 1);
    tape.move_head(TuringTapeMove::Left);
    assert_eq!(tape.current_position, 0);
}

#[test]
fn test_current_position() {
    let mut tape = TuringTape::new(' ');
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::None);
    assert_eq!(tape.current_position, 0);
    tape.move_head(TuringTapeMove::Right);
    assert_eq!(tape.current_position, 1);
    tape.move_head(TuringTapeMove::Left);
    assert_eq!(tape.current_position, 0);
}
