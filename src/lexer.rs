#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Operator {
    pub fn precedence(&self) -> usize {
        match self {
            Operator::Plus | Operator::Minus => 1,
            Operator::Multiply | Operator::Divide => 2,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Token {
    Whitespace,
    Constant(usize, i32),
    Operator(usize, Operator),
    OpenParen(usize),
    ClosingParen(usize),
    End,
    Unknown(usize, char),
}

impl Token {
    pub fn pos(&self) -> usize {
        match self {
            Token::Constant(pos, _)
            | Token::Operator(pos, _)
            | Token::OpenParen(pos)
            | Token::ClosingParen(pos)
            | Token::Unknown(pos, _) => *pos,
            Token::Whitespace | Token::End => 0,
        }
    }
}

pub trait Lexer {
    fn pos(&self) -> usize;
    fn next_token(&mut self) -> Token;
}

pub struct StrLexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> StrLexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    fn consume_whitespace(&mut self) -> Token {
        let start = self.pos;

        match self.source[start..].find(|c: char| !c.is_whitespace()) {
            Some(found) => self.pos += found,
            None => self.pos = self.source.len(),
        };

        Token::Whitespace
    }

    fn consume_constant(&mut self) -> Token {
        let start = self.pos;

        match self.source[start..].find(|c: char| !c.is_numeric()) {
            Some(found) => self.pos += found,
            None => self.pos = self.source.len(),
        };

        Token::Constant(start, self.source[start..self.pos].parse().unwrap())
    }
}

impl<'a> Lexer for StrLexer<'a> {
    fn pos(&self) -> usize {
        self.pos
    }

    fn next_token(&mut self) -> Token {
        let first = match self.source[self.pos..].chars().next() {
            Some(c) => c,
            None => return Token::End,
        };

        match first {
            c if c.is_whitespace() => self.consume_whitespace(),

            c if c.is_numeric() => self.consume_constant(),

            '(' => {
                let pos = self.pos;
                self.pos += 1;
                Token::OpenParen(pos)
            }

            ')' => {
                let pos = self.pos;
                self.pos += 1;
                Token::ClosingParen(pos)
            }

            '+' => {
                let pos = self.pos;
                self.pos += 1;
                Token::Operator(pos, Operator::Plus)
            }

            '-' => {
                let pos = self.pos;
                self.pos += 1;
                Token::Operator(pos, Operator::Minus)
            }

            '*' => {
                let pos = self.pos;
                self.pos += 1;
                Token::Operator(pos, Operator::Multiply)
            }

            '/' => {
                let pos = self.pos;
                self.pos += 1;
                Token::Operator(pos, Operator::Divide)
            }

            _ => {
                let pos = self.pos;
                self.pos += 1;
                Token::Unknown(pos, first)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'a> Iterator for StrLexer<'a> {
        type Item = Token;

        fn next(&mut self) -> Option<Self::Item> {
            match self.next_token() {
                Token::End => None,
                token => Some(token),
            }
        }
    }

    macro_rules! assert_parse {
        ($input:literal, $output:expr) => {{
            let lexer = StrLexer::new($input);
            assert_eq!(lexer.collect::<Vec<Token>>(), $output);
        }};
    }

    #[test]
    fn constants() {
        assert_parse!("10", vec![Token::Constant(0, 10)]);
        assert_parse!(
            "10 20",
            vec![
                Token::Constant(0, 10),
                Token::Whitespace,
                Token::Constant(3, 20)
            ]
        );
    }

    #[test]
    fn symbols() {
        assert_parse!("+", vec![Token::Operator(0, Operator::Plus)]);
        assert_parse!("-", vec![Token::Operator(0, Operator::Minus)]);
        assert_parse!("*", vec![Token::Operator(0, Operator::Multiply)]);
        assert_parse!("/", vec![Token::Operator(0, Operator::Divide)]);
        assert_parse!(
            " + - * / ",
            vec![
                Token::Whitespace,
                Token::Operator(1, Operator::Plus),
                Token::Whitespace,
                Token::Operator(3, Operator::Minus),
                Token::Whitespace,
                Token::Operator(5, Operator::Multiply),
                Token::Whitespace,
                Token::Operator(7, Operator::Divide),
                Token::Whitespace,
            ]
        );
    }

    #[test]
    fn expression() {
        assert_parse!(
            "10 * 20 + 30",
            vec![
                Token::Constant(0, 10),
                Token::Whitespace,
                Token::Operator(3, Operator::Multiply),
                Token::Whitespace,
                Token::Constant(5, 20),
                Token::Whitespace,
                Token::Operator(8, Operator::Plus),
                Token::Whitespace,
                Token::Constant(10, 30),
            ]
        );
    }
}
