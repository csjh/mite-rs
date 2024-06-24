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

enum ProgramBody {
    Statement(Statement),
    ModuleDeclaration(Declaration),
}

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
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.index)
    }

    fn next(&mut self) -> Option<&Token> {
        self.index += 1;
        self.tokens.get(self.index - 1)
    }

    fn parse(&mut self) -> Program {
        let mut body = Vec::new();

        Program { body }
    }
}
