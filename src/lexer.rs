#[derive(Clone, Copy, Debug, Eq, PartialEq)]
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

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    fn next_token(&mut self) -> Token {
        let first = match self.source[self.pos..].chars().next() {
            Some(c) => c,
            None => return Token::End,
        };

        dbg!(first);

        match first {
            c if c.is_numeric() => self.consume_constant(),

            _ => todo!(),
        }
    }

    fn consume_constant(&mut self) -> Token {
        let start = self.pos;

        match self.source[start..].find(|c: char| !c.is_numeric()) {
            Some(found) => self.pos += dbg!(found),
            None => self.pos = self.source.len(),
        };

        dbg!(&self.source[start..self.pos]);

        Token::Constant(self.source[start..self.pos].parse().unwrap())
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Token::End => None,
            t => Some(t),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let lexer = Lexer::new("10");
        dbg!(lexer.collect::<Vec<Token>>());
    }
}
