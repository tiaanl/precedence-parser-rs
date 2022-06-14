use crate::lexer::Operator;
use crate::{Lexer, Token};
use std::fmt::Formatter;
use thiserror::Error;

#[derive(Debug, Eq, PartialEq)]
pub enum Expression {
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),

    Value(i32),
}

impl Expression {
    pub fn evaluate(&self) -> i32 {
        match self {
            Expression::Add(left, right) => left.evaluate() + right.evaluate(),
            Expression::Subtract(left, right) => left.evaluate() - right.evaluate(),
            Expression::Multiply(left, right) => left.evaluate() * right.evaluate(),
            Expression::Divide(left, right) => left.evaluate() / right.evaluate(),
            Expression::Value(value) => *value,
        }
    }
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Add(left, right) => write!(f, "({} + {})", left, right),
            Expression::Subtract(left, right) => write!(f, "({} - {})", left, right),
            Expression::Multiply(left, right) => write!(f, "({} * {})", left, right),
            Expression::Divide(left, right) => write!(f, "({} / {})", left, right),
            Expression::Value(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Error)]
pub enum Expected {
    #[error("Term expected, found {1:?}")]
    Term(usize, Token),
    #[error("Operator expected, got {1:?}")]
    Operator(usize, Token),
    #[error("Closing parenthesis expected")]
    ClosingParen(usize),
}

impl Expected {
    pub fn pos(&self) -> usize {
        match self {
            Expected::Term(pos, _) | Expected::Operator(pos, _) | Expected::ClosingParen(pos) => {
                *pos
            }
        }
    }
}

pub struct Parser<L: Lexer> {
    lexer: L,

    token: Token,
    token_pos: usize,
}

impl<L: Lexer> Parser<L> {
    pub fn new(lexer: L) -> Self {
        let mut parser = Self {
            lexer,
            token: Token::End,
            token_pos: 0,
        };

        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        loop {
            let token = self.lexer.next_token();
            match token {
                Token::Whitespace => {
                    continue;
                }

                _ => {
                    self.token_pos = token.pos();
                    self.token = token;
                    break;
                }
            }
        }
    }

    pub fn parse_expression(&mut self) -> Result<Expression, Expected> {
        self.parse_expression_with_precedence(0)
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: usize,
    ) -> Result<Expression, Expected> {
        let mut left = self.consume_term()?;

        loop {
            match self.token {
                // If there are no more tokens or we've reached a closing paren, then we stop
                // parsing the current expression.
                Token::End | Token::ClosingParen(_) => break,

                Token::Operator(_, operator) => {
                    if operator.precedence() <= precedence {
                        break;
                    }

                    left = self.parse_infix_expression(left)?;
                }

                _ => return Err(Expected::Operator(self.token_pos, self.token)),
            }
        }

        Ok(left)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, Expected> {
        let mut left = left;

        macro_rules! op {
            ($op:ident, $operator:expr) => {{
                let precedence = $operator.precedence();
                self.next_token();
                let right = self.parse_expression_with_precedence(precedence)?;
                left = Expression::$op(Box::new(left), Box::new(right));
            }};
        }

        match self.token {
            Token::Operator(_, operator) => match operator {
                Operator::Plus => op!(Add, operator),
                Operator::Minus => op!(Subtract, operator),
                Operator::Multiply => op!(Multiply, operator),
                Operator::Divide => op!(Divide, operator),
            },

            Token::End => {
                return Ok(left);
            }

            _ => return Err(Expected::Operator(self.token_pos, self.token)),
        }

        Ok(left)
    }

    fn consume_term(&mut self) -> Result<Expression, Expected> {
        match self.token {
            Token::Constant(_, value) => {
                self.next_token();
                Ok(Expression::Value(value))
            }

            Token::OpenParen(_) => self.parse_grouped_expression(),

            _ => Err(Expected::Term(self.token_pos, self.token)),
        }
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression, Expected> {
        // Consume the opening parenthesis.
        self.next_token();

        let expr = self.parse_expression_with_precedence(0)?;

        self.consume_closing_paren()?;

        Ok(expr)
    }

    fn consume_closing_paren(&mut self) -> Result<(), Expected> {
        if let Token::ClosingParen(_) = self.token {
            self.next_token();
            Ok(())
        } else {
            Err(Expected::ClosingParen(self.token.pos()))
        }
    }
}

pub fn parse_expression<L: Lexer>(lexer: L) -> Result<Expression, Expected> {
    let mut parser = Parser::new(lexer);
    parser.parse_expression()
}

#[cfg(test)]
mod tests {
    use super::*;

    struct VecLexer {
        tokens: Vec<Token>,
        index: usize,
    }

    impl VecLexer {
        fn new(tokens: Vec<Token>) -> Self {
            Self { tokens, index: 0 }
        }
    }

    impl Lexer for VecLexer {
        fn pos(&self) -> usize {
            match self.tokens[self.index] {
                Token::Constant(pos, _)
                | Token::Operator(pos, _)
                | Token::OpenParen(pos)
                | Token::ClosingParen(pos)
                | Token::Unknown(pos, _) => pos,
                _ => 0,
            }
        }

        fn next_token(&mut self) -> Token {
            if self.index >= self.tokens.len() {
                Token::End
            } else {
                let token = self.tokens[self.index];
                self.index += 1;
                token
            }
        }
    }

    macro_rules! assert_expr {
        ($input:expr, $output:expr, $answer:literal) => {{
            let mut parser = Parser::new(VecLexer::new($input));
            let expr = parser.parse_expression().unwrap();
            assert_eq!(expr, $output);
            assert_eq!(expr.evaluate(), $answer);
        }};
    }

    #[test]
    fn term_only() {
        assert_expr!(vec![Token::Constant(0, 10)], Expression::Value(10), 10);
    }

    #[test]
    fn single_operator() {
        assert_expr!(
            vec![
                Token::Constant(0, 10),
                Token::Operator(0, Operator::Plus),
                Token::Constant(0, 20)
            ],
            Expression::Add(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            30
        );

        assert_expr!(
            vec![
                Token::Constant(0, 10),
                Token::Operator(0, Operator::Minus),
                Token::Constant(0, 20)
            ],
            Expression::Subtract(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            -10
        );

        assert_expr!(
            vec![
                Token::Constant(0, 10),
                Token::Operator(0, Operator::Multiply),
                Token::Constant(0, 20)
            ],
            Expression::Multiply(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            200
        );

        assert_expr!(
            vec![
                Token::Constant(0, 20),
                Token::Operator(0, Operator::Divide),
                Token::Constant(0, 10)
            ],
            Expression::Divide(
                Box::new(Expression::Value(20)),
                Box::new(Expression::Value(10)),
            ),
            2
        );
    }

    #[test]
    fn associativity() {
        // 1 - 2 + 3
        assert_expr!(
            vec![
                Token::Constant(0, 1),
                Token::Operator(0, Operator::Minus),
                Token::Constant(0, 2),
                Token::Operator(0, Operator::Plus),
                Token::Constant(0, 3),
            ],
            Expression::Add(
                Box::new(Expression::Subtract(
                    Box::new(Expression::Value(1)),
                    Box::new(Expression::Value(2)),
                )),
                Box::new(Expression::Value(3)),
            ),
            2
        );
    }

    #[test]
    fn precedence() {
        // 1 - 2 * 3
        assert_expr!(
            vec![
                Token::Constant(0, 1),
                Token::Operator(0, Operator::Minus),
                Token::Constant(0, 2),
                Token::Operator(0, Operator::Multiply),
                Token::Constant(0, 3),
            ],
            Expression::Subtract(
                Box::new(Expression::Value(1)),
                Box::new(Expression::Multiply(
                    Box::new(Expression::Value(2)),
                    Box::new(Expression::Value(3)),
                )),
            ),
            -5
        );
    }
}
