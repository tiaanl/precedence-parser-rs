use crate::Token;
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
            Expression::Add(left, right) => write!(f, "{} + {}", left, right),
            Expression::Subtract(left, right) => write!(f, "{} - {}", left, right),
            Expression::Multiply(left, right) => write!(f, "{} * {}", left, right),
            Expression::Divide(left, right) => write!(f, "{} / {}", left, right),
            Expression::Value(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Error)]
pub enum ParserError {
    #[error("Term expected")]
    TermExpected,
    #[error("Operator expected")]
    OperatorExpected,
}

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,

    token: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut parser = Self {
            tokens,
            current: 0,
            token: Token::End,
        };

        parser.next_token();

        parser
    }

    fn next_token(&mut self) {
        self.token = match self.tokens.get(self.current) {
            Some(token) => {
                self.current += 1;
                *token
            }
            None => Token::End,
        };
    }

    pub fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_expression_with_precedence(0)
    }

    fn parse_expression_with_precedence(
        &mut self,
        precedence: usize,
    ) -> Result<Expression, ParserError> {
        let mut left = self.consume_term()?;

        loop {
            match self.token {
                Token::End => break,

                Token::Plus | Token::Minus | Token::Star | Token::ForwardSlash => {
                    if self.token.precedence() <= precedence {
                        break;
                    }

                    left = self.parse_infix_expression(left)?;
                }

                _ => return Err(ParserError::OperatorExpected),
            }
        }

        Ok(left)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, ParserError> {
        let mut left = left;

        macro_rules! op {
            ($op:ident) => {{
                let precedence = self.token.precedence();
                self.next_token();
                let right = self.parse_expression_with_precedence(precedence)?;
                left = Expression::$op(Box::new(left), Box::new(right));
            }};
        }

        match self.token {
            Token::Plus => op!(Add),
            Token::Minus => op!(Subtract),
            Token::Star => op!(Multiply),
            Token::ForwardSlash => op!(Divide),

            Token::End => {
                return Ok(left);
            }

            _ => return Err(ParserError::OperatorExpected),
        }

        Ok(left)
    }

    fn consume_term(&mut self) -> Result<Expression, ParserError> {
        match self.token {
            Token::Constant(value) => {
                self.next_token();
                Ok(Expression::Value(value))
            }
            _ => Err(ParserError::TermExpected),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_expr {
        ($input:expr, $output:expr, $answer:literal) => {{
            let mut parser = Parser::new($input);
            let expr = parser.parse_expression().unwrap();
            assert_eq!(expr, $output);
            assert_eq!(expr.evaluate(), $answer);
        }};
    }

    #[test]
    fn term_only() {
        assert_expr!(vec![Token::Constant(10)], Expression::Value(10), 10);
    }

    #[test]
    fn single_operator() {
        assert_expr!(
            vec![Token::Constant(10), Token::Plus, Token::Constant(20)],
            Expression::Add(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            30
        );

        assert_expr!(
            vec![Token::Constant(10), Token::Minus, Token::Constant(20)],
            Expression::Subtract(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            -10
        );

        assert_expr!(
            vec![Token::Constant(10), Token::Star, Token::Constant(20)],
            Expression::Multiply(
                Box::new(Expression::Value(10)),
                Box::new(Expression::Value(20)),
            ),
            200
        );

        assert_expr!(
            vec![
                Token::Constant(20),
                Token::ForwardSlash,
                Token::Constant(10)
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
                Token::Constant(1),
                Token::Minus,
                Token::Constant(2),
                Token::Plus,
                Token::Constant(3),
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
                Token::Constant(1),
                Token::Minus,
                Token::Constant(2),
                Token::Star,
                Token::Constant(3),
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
