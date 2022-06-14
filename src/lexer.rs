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
    End,
}

pub struct Lexer<'a> {
    source: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        Self { source, pos: 0 }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Token {
        let first = match self.source[self.pos..].chars().next() {
            Some(c) => c,
            None => return Token::End,
        };

        return match first {
            c if c.is_whitespace() => self.consume_whitespace(),

            c if c.is_numeric() => self.consume_constant(),

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

            _ => todo!(),
        };
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

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_parse {
        ($input:literal, $output:expr) => {{
            let lexer = Lexer::new($input);
            assert_eq!(lexer.collect::<Vec<Token>>(), $output);
        }};
    }

    #[test]
    fn constants() {
        assert_parse!("10", vec![Token::Constant(10)]);
        assert_parse!(
            "10 20",
            vec![Token::Constant(10), Token::Whitespace, Token::Constant(20)]
        );
    }

    #[test]
    fn symbols() {
        assert_parse!("+", vec![Token::Operator(Operator::Plus)]);
        assert_parse!("-", vec![Token::Operator(Operator::Minus)]);
        assert_parse!("*", vec![Token::Operator(Operator::Multiply)]);
        assert_parse!("/", vec![Token::Operator(Operator::Divide)]);
        assert_parse!(
            " + - * / ",
            vec![
                Token::Whitespace,
                Token::Operator(Operator::Plus),
                Token::Whitespace,
                Token::Operator(Operator::Minus),
                Token::Whitespace,
                Token::Operator(Operator::Multiply),
                Token::Whitespace,
                Token::Operator(Operator::Divide),
                Token::Whitespace,
            ]
        );
    }

    #[test]
    fn expression() {
        assert_parse!(
            "10 * 20 + 30",
            vec![
                Token::Constant(10),
                Token::Whitespace,
                Token::Operator(Operator::Multiply),
                Token::Whitespace,
                Token::Constant(20),
                Token::Whitespace,
                Token::Operator(Operator::Plus),
                Token::Whitespace,
                Token::Constant(30),
            ]
        );
    }
}
