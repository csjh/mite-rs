#[derive(Debug, Clone, PartialEq)]
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

    let mut chars = input.chars().peekable();

    // tokenize the input
    while let Some(char) = chars.find(|c| !c.is_whitespace()) {
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
            '=' => Token::Assignment,
            '!' => Token::Not,
            '<' => Token::LessThan,
            '>' => Token::GreaterThan,
            '|' => Token::BitwiseOr,
            '^' => Token::BitwiseXor,
            '&' => Token::BitwiseAnd,

            '"' => {
                let mut string = String::new();
                while let Some(char) = chars.next() {
                    if char == '\\' {
                        match chars.next() {
                            Some('n') => string.push('\n'),
                            Some('r') => string.push('\r'),
                            Some('t') => string.push('\t'),
                            Some('\\') => string.push('\\'),
                            Some('"') => string.push('"'),
                            Some('0') => string.push('\0'),
                            Some(_) => panic!("unexpected escape character"),
                            None => panic!("unexpected end of input"),
                        }
                    } else if char == '"' {
                        break;
                    } else {
                        string.push(char);
                    }
                }
                Token::String(string)
            }

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
        let peeked = chars.peek().unwrap_or(&'\0');
        let token = match (&token, peeked) {
            (Token::Identifier(ref idtfr), _) => match idtfr.as_str() {
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
            (Token::Plus, '=') => {
                chars.next();
                Token::AssignmentPlus
            }
            (Token::Minus, '=') => {
                chars.next();
                Token::AssignmentMinus
            }
            (Token::Slash, '=') => {
                chars.next();
                Token::AssignmentSlash
            }
            (Token::Star, '=') => {
                chars.next();
                Token::AssignmentStar
            }
            (Token::Remainder, '=') => {
                chars.next();
                Token::AssignmentRemainder
            }
            (Token::BitwiseOr, '=') => {
                chars.next();
                Token::AssignmentBitwiseOr
            }
            (Token::BitwiseOr, '|') => {
                chars.next();
                Token::LogicalOr
            }
            (Token::BitwiseXor, '=') => {
                chars.next();
                Token::AssignmentBitwiseXor
            }
            (Token::BitwiseAnd, '=') => {
                chars.next();
                Token::AssignmentBitwiseAnd
            }
            (Token::BitwiseAnd, '&') => {
                chars.next();
                Token::LogicalAnd
            }
            (Token::Assignment, '=') => {
                chars.next();
                Token::Equals
            }
            (Token::LessThan, '=') => {
                chars.next();
                Token::LessThanEquals
            }
            (Token::LessThan, '<') => {
                chars.next();
                Token::BitshiftLeft
            }
            (Token::GreaterThan, '=') => {
                chars.next();
                Token::GreaterThanEquals
            }
            (Token::GreaterThan, '>') => {
                chars.next();
                Token::BitshiftRight
            }
            (Token::Not, '=') => {
                chars.next();
                Token::NotEquals
            }
            _ => token,
        };

        // check for 3-character tokens
        let peeked = chars.peek().unwrap_or(&'\0');
        let token = match (&token, peeked) {
            (Token::BitshiftRight, '=') => {
                chars.next();
                Token::AssignmentBitshiftRight
            }
            (Token::BitshiftLeft, '=') => {
                chars.next();
                Token::AssignmentBitshiftLeft
            }
            _ => token,
        };

        tokens.push(token);
    }

    tokens.push(Token::EOF);

    tokens
}
