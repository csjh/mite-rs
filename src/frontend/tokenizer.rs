#[derive(Debug, PartialEq)]
pub(crate) enum Token {
    Identifier(String),
    Number(String),
    String(String),

    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    SemiColon,
    Period,

    Plus,
    Minus,
    Slash,
    Star,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    BitshiftLeft,
    BitshiftRight,
    Remainder,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    LogicalAnd,
    LogicalOr,

    Not,
    BitwiseNot,

    Assignment,
    AssignmentPlus,
    AssignmentMinus,
    AssignmentSlash,
    AssignmentStar,
    AssignmentBitshiftLeft,
    AssignmentBitshiftRight,
    AssignmentRemainder,
    AssignmentBitwiseOr,
    AssignmentBitwiseXor,
    AssignmentBitwiseAnd,

    Ref,
    Let,
    Const,
    Fn,
    Export,
    Struct,
    Return,
    If,
    Else,
    For,
    Do,
    While,
    Continue,
    Break,
    Import,
    As,
    From,

    EOF,
}

pub fn tokenize(input: &str) -> Vec<Token> {
    let mut tokens = vec![];

    let mut chars = input
        .chars()
        .filter(|c: &char| !c.is_whitespace())
        .peekable();

    // tokenize the input
    while let Some(char) = chars.next() {
        let token = match char {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            '[' => Token::LeftBracket,
            ']' => Token::RightBracket,
            ':' => Token::Colon,
            ',' => Token::Comma,
            ';' => Token::SemiColon,
            '.' => Token::Period,

            '+' => Token::Plus,
            '-' => Token::Minus,
            '/' => Token::Slash,
            '*' => Token::Star,
            '=' => Token::Equals,
            '!' => Token::Not,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '|' => Token::BitwiseOr,
            '^' => Token::BitwiseXor,
            '&' => Token::BitwiseAnd,

            _ => {
                if char.is_digit(10) {
                    let mut num = char.to_string();
                    while let Some(char) = chars.peek() {
                        if char.is_digit(10) {
                            num.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    Token::Number(num)
                } else if char.is_alphabetic() {
                    let mut identifier = char.to_string();
                    while let Some(char) = chars.peek() {
                        if char.is_alphanumeric() || *char == '_' {
                            identifier.push(chars.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    Token::Identifier(identifier)
                } else {
                    panic!("unexpected character: {}", char);
                }
            }
        };

        // check for 2-character tokens
        let peeked = *chars.peek().get_or_insert(&'\0');
        let token = match token {
            Token::Identifier(ref idtfr) => match idtfr.as_str() {
                "let" => Token::Let,
                "const" => Token::Const,
                "fn" => Token::Fn,
                "export" => Token::Export,
                "struct" => Token::Struct,
                "return" => Token::Return,
                "if" => Token::If,
                "else" => Token::Else,
                "for" => Token::For,
                "do" => Token::Do,
                "while" => Token::While,
                "continue" => Token::Continue,
                "break" => Token::Break,
                "import" => Token::Import,
                "as" => Token::As,
                "from" => Token::From,
                _ => token,
            },
            Token::Plus => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentPlus
                }
                _ => token,
            },
            Token::Minus => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentMinus
                }
                _ => token,
            },
            Token::Slash => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentSlash
                }
                _ => token,
            },
            Token::Star => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentStar
                }
                _ => token,
            },
            Token::Remainder => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentRemainder
                }
                _ => token,
            },
            Token::BitwiseOr => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentBitwiseOr
                }
                '|' => {
                    chars.next();
                    Token::LogicalOr
                }
                _ => token,
            },
            Token::BitwiseXor => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentBitwiseXor
                }
                _ => token,
            },
            Token::BitwiseAnd => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentBitwiseAnd
                }
                '&' => {
                    chars.next();
                    Token::LogicalAnd
                }
                _ => token,
            },
            Token::Assignment => match peeked {
                '=' => {
                    chars.next();
                    Token::Equals
                }
                _ => token,
            },
            Token::LessThan => match peeked {
                '=' => {
                    chars.next();
                    Token::LessThanEquals
                }
                '<' => {
                    chars.next();
                    Token::BitshiftLeft
                }
                _ => token,
            },
            Token::GreaterThan => match peeked {
                '=' => {
                    chars.next();
                    Token::GreaterThanEquals
                }
                '>' => {
                    chars.next();
                    Token::BitshiftRight
                }
                _ => token,
            },
            Token::Not => match peeked {
                '=' => {
                    chars.next();
                    Token::NotEquals
                }
                _ => token,
            },
            _ => token,
        };

        // check for 3-character tokens
        let peeked = *chars.peek().get_or_insert(&'\0');
        let token = match token {
            Token::BitshiftRight => match peeked {
                '=' => {
                    chars.next();
                    Token::AssignmentBitshiftRight
                }
                _ => token,
            },
            _ => token,
        };

        tokens.push(token);
    }

    tokens.push(Token::EOF);

    tokens
}
