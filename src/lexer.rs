#[derive(Clone, Copy, Debug)]
pub enum Token {
    Constant(i32),
    Plus,
    Minus,
    Star,
    ForwardSlash,
    End,
}

impl Token {
    pub fn precedence(&self) -> usize {
        match self {
            Token::Plus | Token::Minus => 1,
            Token::Star | Token::ForwardSlash => 2,

            Token::Constant(_) | Token::End => unreachable!(),
        }
    }
}
