use crate::frontend::tokenizer::Token;

const unary_operators: [Token; 4] = [Token::Not, Token::BitwiseNot, Token::Plus, Token::Minus];
const precedence: [&'static [Token]; 11] = [
    &[Token::Star, Token::Slash, Token::Remainder],
    &[Token::Plus, Token::Minus],
    &[Token::BitshiftLeft, Token::BitshiftRight],
    &[
        Token::LessThan,
        Token::LessThanEquals,
        Token::GreaterThan,
        Token::GreaterThanEquals,
    ],
    &[Token::Equals, Token::NotEquals],
    &[Token::BitwiseAnd],
    &[Token::BitwiseXor],
    &[Token::BitwiseOr],
    &[Token::LogicalAnd],
    &[Token::LogicalOr],
    &[
        Token::Assignment,
        Token::AssignmentPlus,
        Token::AssignmentMinus,
        Token::AssignmentStar,
        Token::AssignmentSlash,
        Token::AssignmentRemainder,
        Token::AssignmentBitwiseAnd,
        Token::AssignmentBitwiseXor,
        Token::AssignmentBitwiseOr,
        Token::AssignmentBitshiftLeft,
        Token::AssignmentBitshiftRight,
    ],
];

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

type Identifier = String;
enum TypeIdentifier {
    Identifier(String),
    Array {
        element_type: Box<TypeIdentifier>,
        size: usize,
    },
    Function {
        parameters: Vec<TypedParameter>,
        return_type: Box<TypeIdentifier>,
    },
}

type ProgramBody = Declaration;

struct Program {
    body: Vec<ProgramBody>,
}

struct TypedParameter {
    name: Identifier,
    typeAnnotation: TypeIdentifier,
}

struct FunctionDeclaration {
    name: Identifier,
    parameters: Vec<TypedParameter>,
    return_type: TypeIdentifier,
    body: Expression,
}

struct Property {
    key: Identifier,
    value: Expression,
}

enum VariableDeclarator {
    Init {
        id: Identifier,
        init: Expression,
    },
    Type {
        id: Identifier,
        typeAnnotation: TypeIdentifier,
    },
    Both {
        id: Identifier,
        init: Expression,
        typeAnnotation: TypeIdentifier,
    },
}

enum Literal {
    string(String),
    f32(f32),
    f64(f64),
    i32(i32),
    u32(u32),
    i64(i64),
    u64(u64),
    bool(bool),
    f64x2([f64; 2]),
    f32x4([f32; 4]),
    i8x16([i8; 16]),
    u8x16([u8; 16]),
    i16x8([i16; 8]),
    u16x8([u16; 8]),
    i32x4([i32; 4]),
    u32x4([u32; 4]),
    i64x2([i64; 2]),
    u64x2([u64; 2]),
}

struct StructField {
    name: Identifier,
    typeAnnotation: TypeIdentifier,
}

struct ImportSpecifier {
    local: Identifier,
    imported: Identifier,
    typeAnnotation: Option<TypeIdentifier>,
}

enum VariableKind {
    Let,
    Const,
}

enum Declaration {
    FunctionDeclaration {
        name: Identifier,
        parameters: Vec<TypedParameter>,
        return_type: TypeIdentifier,
        body: Expression,
    },
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: VariableKind,
    },
    StructDeclaration {
        id: Identifier,
        fields: Vec<StructField>,
        methods: Vec<FunctionDeclaration>,
    },
    ExportNamedDeclaration {
        declaration: Box<Declaration>,
    },
    ImportDeclaration {
        source: Literal,
        specifiers: Vec<ImportSpecifier>,
    },
}

enum Statement {
    ExpressionStatement { expression: Expression },
    ReturnStatement,
    Declaration(Declaration),
}

enum ForExpressionInit {
    VariableDeclaration(Declaration),
    Expression(Expression),
}

enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    BlockExpression {
        body: Vec<Statement>,
    },
    BreakExpression,
    ContinueExpression,
    WhileExpression {
        test: Box<Expression>,
        body: Box<Expression>,
    },
    DoWhileExpression {
        test: Box<Expression>,
        body: Box<Expression>,
    },
    ForExpression {
        init: Option<Box<ForExpressionInit>>,
        test: Option<Box<Expression>>,
        update: Option<Box<Expression>>,
        body: Box<Expression>,
    },
    EmptyExpression,
    ArrayExpression {
        elements: Vec<Expression>,
    },
    ObjectExpression {
        typeAnnotation: TypeIdentifier,
        properties: Vec<Property>,
    },
    SequenceExpression {
        expressions: Vec<Expression>,
    },
    UnaryExpression {
        operator: Token,
        argument: Box<Expression>,
    },
    BinaryExpression {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    AssignmentExpression {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    LogicalExpression {
        operator: Token,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    IfExpression {
        test: Box<Expression>,
        consequent: Box<Expression>,
        alternate: Option<Box<Expression>>,
    },
    CallExpression {
        callee: Box<Expression>,
        arguments: Vec<Expression>,
    },
    MemberExpression {
        object: Box<Expression>,
        property: Identifier,
    },
    IndexExpression {
        object: Box<Expression>,
        index: Box<Expression>,
    },
}

impl Parser {
    fn parse(&mut self) -> Program {
        let mut body = Vec::new();
        loop {
            match self.peek() {
                Token::EOF => break,
                _ => {
                    let result = self.parseTopLevelDeclaration();
                    match result {
                        Ok(res) => body.push(res),
                        Err(err) => {
                            eprintln!("{}", err);
                            break;
                        }
                    }
                }
            };
        }
        Program { body }
    }

    fn parseTopLevelDeclaration(&mut self) -> Result<ProgramBody, String> {
        let token = self.peek();
        match token {
            Token::Fn => self.parseFunction(),
            Token::Struct => self.parseStruct(),
            Token::Export => self.parseExport(),
            Token::Import => self.parseImport(),
            Token::Let | Token::Const => {
                let decl = self.parseVariableDeclaration();
                self.eatToken(Token::SemiColon);
                decl
            }
            _ => Err(format!("unexpected token at top level: {:?}", token)),
        }
    }
    fn parseImport(&mut self) -> Result<ProgramBody, String> {}
    fn parseExport(&mut self) -> Result<ProgramBody, String> {}
    fn parseStruct(&mut self) -> Result<ProgramBody, String> {}
    fn parseFunction(&mut self) -> Result<ProgramBody, String> {}
    fn parseVariableDeclaration(&mut self) -> Result<ProgramBody, String> {}

    fn parseStatement(&mut self) -> Result<Statement, String> {}

    fn parseExpression(&mut self) -> Result<Expression, String> {}
    fn parseIfExpression(&mut self) -> Result<Expression, String> {}
    fn parseForExpression(&mut self) -> Result<Expression, String> {}
    fn parseDoWhileExpression(&mut self) -> Result<Expression, String> {}
    fn parseWhileExpression(&mut self) -> Result<Expression, String> {}
    fn parseSequenceExpression(&mut self) -> Result<Expression, String> {}
    fn parseBlockExpression(&mut self) -> Result<Expression, String> {}
    fn parseIdentifier(&mut self) -> Result<Expression, String> {}
    fn parseNumberLiteral(&mut self) -> Result<Expression, String> {}
    fn parseSIMDLiteral(&mut self) -> Result<Expression, String> {}
    fn parseStringLiteral(&mut self) -> Result<Expression, String> {}
    fn parseStructLiteral(&mut self) -> Result<Expression, String> {}
    fn parseArrayLiteral(&mut self) -> Result<Expression, String> {}
    fn parseCallExpression(&mut self) -> Result<Expression, String> {}
    fn parseMemberExpression(&mut self) -> Result<Expression, String> {}
    fn parseIndexExpression(&mut self) -> Result<Expression, String> {}
    fn parseEmptyExpression(&mut self) -> Result<Expression, String> {}
    fn parseContinueExpression(&mut self) -> Result<Expression, String> {}
    fn parseBreakExpression(&mut self) -> Result<Expression, String> {}
    fn parseUnaryExpression(&mut self) -> Result<Expression, String> {}

    fn parseType(&mut self) -> Result<TypeIdentifier, String> {}

    fn constructBinaryExpression(
        &mut self,
        left: Expression,
        operator: Token,
        right: Expression,
    ) -> Result<Expression, String> {
    }

    fn expectToken(&mut self, token: Token) -> Result<(), String> {
        if *self.peek() == token {
            Ok(())
        } else {
            Err(format!("expected token {:?}, got {:?}", token, self.peek()))
        }
    }

    fn eatToken(&mut self, token: Token) -> Result<(), String> {
        self.expectToken(token)?;
        self.next();
        Ok(())
    }

    fn takeToken(&mut self, token: Token) -> Result<&Token, String> {
        self.expectToken(token)?;
        Ok(self.next())
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.index).unwrap_or(&Token::EOF)
    }

    fn next(&mut self) -> &Token {
        self.index += 1;
        self.tokens.get(self.index - 1).unwrap_or(&Token::EOF)
    }
}
