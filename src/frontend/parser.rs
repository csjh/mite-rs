use crate::frontend::tokenizer::Token;

const UNARY_OPERATORS: [Token; 4] = [Token::Not, Token::BitwiseNot, Token::Plus, Token::Minus];
const PRECEDENCE: [&'static [Token]; 11] = [
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

pub(crate) type Identifier = String;

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum TypeIdentifier {
    Identifier {
        is_ref: bool,
        name: String,
    },
    Array {
        is_ref: bool,
        element_type: Box<TypeIdentifier>,
        size: u32,
    },
    Function {
        is_ref: bool,
        parameters: Vec<TypedParameter>,
        return_type: Box<TypeIdentifier>,
    },
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Program {
    pub body: Vec<Declaration>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct TypedParameter {
    pub name: Identifier,
    pub type_annotation: TypeIdentifier,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct FunctionDeclaration {
    pub name: Identifier,
    pub parameters: Vec<TypedParameter>,
    pub return_type: TypeIdentifier,
    pub body: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Property {
    pub key: Identifier,
    pub value: Expression,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct VariableDeclarator {
    pub id: Identifier,
    pub init: Option<Expression>,
    pub type_annotation: Option<TypeIdentifier>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Literal {
    String(String),
    // f32(f32),
    F64(f64),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    // bool(bool),
    F64x2([f64; 2]),
    F32x4([f32; 4]),
    I8x16([i8; 16]),
    U8x16([u8; 16]),
    I16x8([i16; 8]),
    U16x8([u16; 8]),
    I32x4([i32; 4]),
    U32x4([u32; 4]),
    I64x2([i64; 2]),
    U64x2([u64; 2]),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct StructField {
    pub name: Identifier,
    pub type_annotation: TypeIdentifier,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct ImportSpecifier {
    pub local: Identifier,
    pub imported: Identifier,
    pub type_annotation: Option<TypeIdentifier>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum VariableKind {
    Let,
    Const,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: VariableKind,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct StructDeclaration {
    pub id: Identifier,
    pub fields: Vec<StructField>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct ImportDeclaration {
    pub source: String,
    pub specifiers: Vec<ImportSpecifier>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Declaration {
    Function(FunctionDeclaration, bool),
    Variable(VariableDeclaration, bool),
    Struct(StructDeclaration, bool),
    Import(ImportDeclaration, bool),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Statement {
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum UnaryOperator {
    Not,
    BitwiseNot,
    Plus,
    Minus,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum BinaryOperator {
    Star,
    Slash,
    Remainder,
    Plus,
    Minus,
    BitshiftLeft,
    BitshiftRight,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    Equals,
    NotEquals,
    And,
    Xor,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum AssignmentOperator {
    Base,
    Plus,
    Minus,
    Star,
    Slash,
    Remainder,
    BitwiseAnd,
    BitwiseXor,
    BitwiseOr,
    BitshiftLeft,
    BitshiftRight,
}

impl Into<BinaryOperator> for AssignmentOperator {
    fn into(self) -> BinaryOperator {
        match self {
            AssignmentOperator::Plus => BinaryOperator::Plus,
            AssignmentOperator::Minus => BinaryOperator::Minus,
            AssignmentOperator::Star => BinaryOperator::Star,
            AssignmentOperator::Slash => BinaryOperator::Slash,
            AssignmentOperator::Remainder => BinaryOperator::Remainder,
            AssignmentOperator::BitwiseAnd => BinaryOperator::And,
            AssignmentOperator::BitwiseXor => BinaryOperator::Xor,
            AssignmentOperator::BitwiseOr => BinaryOperator::Or,
            AssignmentOperator::BitshiftLeft => BinaryOperator::BitshiftLeft,
            AssignmentOperator::BitshiftRight => BinaryOperator::BitshiftRight,
            AssignmentOperator::Base => panic!("cannot convert base assignment operator to binary"),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum LogicalOperator {
    And,
    Or,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Block {
    pub body: Vec<Statement>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct While {
    pub test: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct DoWhile {
    pub test: Box<Expression>,
    pub body: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct For {
    pub init: Option<Box<Statement>>,
    pub test: Option<Box<Expression>>,
    pub update: Option<Box<Expression>>,
    pub body: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Array {
    pub elements: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Object {
    pub type_annotation: TypeIdentifier,
    pub properties: Vec<Property>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Sequence {
    pub expressions: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Unary {
    pub operator: UnaryOperator,
    pub argument: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Binary {
    pub operator: BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Assignment {
    pub operator: AssignmentOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Logical {
    pub operator: LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct If {
    pub test: Box<Expression>,
    pub consequent: Box<Expression>,
    pub alternate: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Call {
    pub callee: Box<Expression>,
    pub arguments: Vec<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Member {
    pub object: Box<Expression>,
    pub property: Identifier,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Index {
    pub object: Box<Expression>,
    pub index: Box<Expression>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Return {
    pub argument: Option<Box<Expression>>,
}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Break {}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Continue {}

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Empty {}

#[derive(Clone, PartialEq, Debug)]
pub(crate) enum Expression {
    Identifier(Identifier),
    Literal(Literal),
    Block(Block),
    Break(Break),
    Continue(Continue),
    While(While),
    DoWhile(DoWhile),
    For(For),
    Empty(Empty),
    Array(Array),
    Object(Object),
    Sequence(Sequence),
    Unary(Unary),
    Binary(Binary),
    Assignment(Assignment),
    Logical(Logical),
    If(If),
    Call(Call),
    Member(Member),
    Index(Index),
    Return(Return),
}

pub fn parse(tokens: Vec<Token>) -> Program {
    Parser::parse(tokens)
}

struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    fn parse(tokens: Vec<Token>) -> Program {
        let mut parser = Parser { tokens, index: 0 };
        parser._parse()
    }

    fn _parse(&mut self) -> Program {
        let mut body = Vec::new();
        loop {
            match self.peek() {
                Token::EOF => break,
                _ => {
                    let result = self.parse_top_level_declaration();
                    match result {
                        Ok(res) => body.push(res),
                        Err(err) => {
                            eprintln!("{}", err);
                            eprintln!(
                                "happened at token: {:?} at idx {:?}",
                                self.peek(),
                                self.index
                            );
                            break;
                        }
                    }
                }
            };
        }

        self.expect_token(Token::EOF).unwrap();

        Program { body }
    }

    fn parse_top_level_declaration(&mut self) -> Result<Declaration, String> {
        match self.peek() {
            Token::Fn => Ok(Declaration::Function(self.parse_function(false)?, false)),
            Token::Struct => self.parse_struct(),
            Token::Export => self.parse_export(),
            Token::Import => self.parse_import(),
            Token::Let | Token::Const => {
                let decl = self.parse_variable_declaration()?;
                self.eat_token(Token::SemiColon)?;
                Ok(Declaration::Variable(decl, false))
            }
            token => Err(format!("unexpected token at top level: {:?}", token)),
        }
    }
    fn parse_import(&mut self) -> Result<Declaration, String> {
        self.eat_token(Token::Import)?;
        self.eat_token(Token::LeftBrace)?;

        let mut specifiers = Vec::new();
        loop {
            let imported = self.parse_identifier()?;

            let local = if matches!(self.peek(), Token::As) {
                self.eat_token(Token::As)?;
                self.parse_identifier()?
            } else {
                imported.clone()
            };

            let specifier = ImportSpecifier {
                local,
                imported,
                type_annotation: if matches!(self.peek(), Token::Colon) {
                    self.eat_token(Token::Colon)?;
                    Some(self.parse_type()?)
                } else {
                    None
                },
            };

            specifiers.push(specifier);

            if matches!(self.peek(), Token::RightBrace) {
                break;
            }

            self.eat_token(Token::Comma)?;
        }

        self.eat_token(Token::RightBrace)?;
        self.eat_token(Token::From)?;

        let source = self.parse_string_literal()?;

        self.eat_token(Token::SemiColon)?;

        Ok(Declaration::Import(
            ImportDeclaration { source, specifiers },
            false,
        ))
    }
    fn parse_export(&mut self) -> Result<Declaration, String> {
        self.eat_token(Token::EOF)?;

        Ok(match self.parse_top_level_declaration()? {
            Declaration::Function(f, _) => Declaration::Function(f, true),
            Declaration::Variable(v, _) => Declaration::Variable(v, true),
            Declaration::Struct(s, _) => Declaration::Struct(s, true),
            Declaration::Import(_, _) => panic!("cannot export import"),
        })
    }
    fn parse_struct(&mut self) -> Result<Declaration, String> {
        self.eat_token(Token::Struct)?;

        let id = self.parse_identifier()?;

        self.eat_token(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        loop {
            match self.peek() {
                Token::RightBrace => break,
                Token::Identifier(_) => match self.get_token(self.index + 1) {
                    Token::Colon => {
                        let name = self.parse_identifier()?;
                        self.eat_token(Token::Colon)?;
                        let type_annotation = self.parse_type()?;

                        fields.push(StructField {
                            name,
                            type_annotation,
                        });
                    }
                    Token::LeftParen => {
                        let mut func = self.parse_function(true)?;
                        func.parameters.insert(
                            0,
                            TypedParameter {
                                name: "this".to_string(),
                                type_annotation: TypeIdentifier::Identifier {
                                    is_ref: true,
                                    name: id.clone(),
                                },
                            },
                        );
                        methods.push(func);
                    }
                    _ => return Err(format!("unexpected token in struct: {:?}", self.peek())),
                },
                _ => return Err(format!("unexpected token in struct: {:?}", self.peek())),
            }
        }

        self.eat_token(Token::RightBrace)?;

        Ok(Declaration::Struct(
            StructDeclaration {
                id,
                fields,
                methods,
            },
            false,
        ))
    }
    fn parse_function(&mut self, is_method: bool) -> Result<FunctionDeclaration, String> {
        if !is_method {
            self.eat_token(Token::Fn)?;
        }

        let name = self.parse_identifier()?;

        self.eat_token(Token::LeftParen)?;

        let mut parameters = Vec::new();
        loop {
            match self.peek() {
                Token::RightParen => break,
                Token::Comma => {
                    self.eat_token(Token::Comma)?;
                }
                _ => {
                    let name = self.parse_identifier()?;
                    self.eat_token(Token::Colon)?;
                    let type_annotation = self.parse_type()?;

                    parameters.push(TypedParameter {
                        name,
                        type_annotation,
                    });
                }
            }
        }

        self.eat_token(Token::RightParen)?;
        self.eat_token(Token::Colon)?;

        let return_type = self.parse_type()?;
        let body = self.parse_block_expression()?;

        Ok(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }
    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration, String> {
        let kind = match self.peek() {
            Token::Let => VariableKind::Let,
            Token::Const => VariableKind::Const,
            token => {
                return Err(format!(
                    "unexpected token in variable declaration: {:?}",
                    token
                ))
            }
        };

        self.eat_token(self.peek().clone())?;

        let mut declarations = Vec::new();
        loop {
            let id = self.parse_identifier()?;

            let type_annotation = if matches!(self.peek(), Token::Colon) {
                self.eat_token(Token::Colon)?;
                Some(self.parse_type()?)
            } else {
                None
            };

            let init = if matches!(self.peek(), Token::Assignment) {
                self.eat_token(Token::Assignment)?;
                Some(self.parse_expression()?)
            } else {
                None
            };

            if init.is_none() && type_annotation.is_none() {
                return Err("variable declaration must have a type or an initializer".to_string());
            }

            declarations.push(VariableDeclarator {
                id,
                init,
                type_annotation,
            });

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        Ok(VariableDeclaration { declarations, kind })
    }

    fn parse_statement(&mut self) -> Result<Statement, String> {
        let statement;

        match self.peek() {
            Token::Let | Token::Const => {
                statement = Statement::VariableDeclaration(self.parse_variable_declaration()?);
            }
            _ => {
                statement = Statement::Expression(self.parse_expression()?);
            }
        }

        self.eat_token(Token::SemiColon)?;

        Ok(statement)
    }

    fn parse_expression(&mut self) -> Result<Expression, String> {
        let mut expression_stack = Vec::new();
        let mut operator_stack = Vec::new();

        if matches!(self.peek(), Token::Let | Token::Const) {
            self.index -= 1;
            return Ok(Expression::Block(Block {
                body: vec![self.parse_statement()?],
            }));
        }

        loop {
            match self.peek() {
                Token::SemiColon | Token::RightParen => {
                    expression_stack.push(self.parse_empty_expression()?)
                }
                Token::LeftBracket => expression_stack.push(self.parse_array_literal()?),
                Token::If => expression_stack.push(self.parse_if_expression()?),
                Token::LeftBrace => {
                    if matches!(self.get_token(self.index + 1), Token::Number(_)) {
                        expression_stack.push(self.parse_simd_literal()?);
                    } else {
                        expression_stack.push(self.parse_block_expression()?);
                    }
                }
                Token::LeftParen => expression_stack.push(self.parse_sequence_expression()?),
                Token::For => expression_stack.push(self.parse_for_expression()?),
                Token::Do => expression_stack.push(self.parse_do_while_expression()?),
                Token::While => expression_stack.push(self.parse_while_expression()?),
                Token::Continue => expression_stack.push(self.parse_continue_expression()?),
                Token::Break => expression_stack.push(self.parse_break_expression()?),
                Token::Number(_) => expression_stack.push(self.parse_number_literal()?),
                Token::String(_) => expression_stack.push(Expression::Literal(Literal::String(
                    self.parse_string_literal()?,
                ))),
                Token::Identifier(_) => {
                    let next = self.parse_identifier()?;
                    if matches!(self.get_token(self.index + 1), Token::LeftBrace) {
                        expression_stack.push(self.parse_struct_literal(next)?);
                    } else {
                        expression_stack.push(Expression::Identifier(next));
                    }
                }
                Token::Not | Token::BitwiseNot | Token::Plus | Token::Minus => {
                    expression_stack.push(self.parse_unary_expression()?);
                }
                Token::Return => {
                    expression_stack.push(self.parse_return_expression()?);
                }
                _ => (),
            }

            loop {
                let last = expression_stack.pop().unwrap();
                match self.peek() {
                    Token::LeftBracket => expression_stack.push(self.parse_index_expression(last)?),
                    Token::Period => expression_stack.push(self.parse_member_expression(last)?),
                    Token::LeftParen => expression_stack.push(self.parse_call_expression(last)?),
                    _ => {
                        expression_stack.push(last);
                        break;
                    }
                }
            }

            match self.peek() {
                Token::SemiColon
                | Token::RightParen
                | Token::RightBracket
                | Token::RightBrace
                | Token::Comma
                | Token::Else
                | Token::While => break,
                Token::Assignment
                | Token::AssignmentPlus
                | Token::AssignmentMinus
                | Token::AssignmentStar
                | Token::AssignmentSlash
                | Token::AssignmentRemainder
                | Token::AssignmentBitwiseAnd
                | Token::AssignmentBitwiseXor
                | Token::AssignmentBitwiseOr
                | Token::AssignmentBitshiftLeft
                | Token::AssignmentBitshiftRight
                | Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Star
                | Token::Equals
                | Token::NotEquals
                | Token::LessThan
                | Token::LessThanEquals
                | Token::GreaterThan
                | Token::GreaterThanEquals
                | Token::BitshiftLeft
                | Token::BitshiftRight
                | Token::BitwiseOr
                | Token::BitwiseXor
                | Token::BitwiseAnd
                | Token::LogicalAnd
                | Token::LogicalOr => {
                    operator_stack.push(self.next().clone());
                }
                _ => {
                    return Err(format!(
                        "unexpected token in expression: {:?}; expected operator",
                        self.peek()
                    ))
                }
            }
        }

        assert!(expression_stack.len() == operator_stack.len() + 1);

        for (j, precedence_level) in PRECEDENCE.iter().enumerate() {
            let right_to_left = j == PRECEDENCE.len() - 1;

            let mut new_expression_stack = Vec::new();
            // probably not needed
            // if expression_stack.len() == 0 {
            //    expression_stack.push(Expression::EmptyExpression);
            // }
            let mut new_operator_stack = Vec::new();

            if !right_to_left {
                new_expression_stack.push(expression_stack.first().unwrap().clone());
                for i in 0..operator_stack.len() {
                    let operator = operator_stack[i].clone();
                    let left = new_expression_stack.pop().unwrap();
                    let right = expression_stack[i + 1].clone();

                    if precedence_level.contains(&operator) {
                        let new_expression =
                            self.construct_binary_expression(left, operator.clone(), right)?;

                        new_expression_stack.push(new_expression);
                    } else {
                        new_expression_stack.push(left);
                        new_expression_stack.push(right);
                        new_operator_stack.push(operator);
                    }
                }
            } else {
                new_expression_stack.push(expression_stack.last().unwrap().clone());
                for i in (0..operator_stack.len()).rev() {
                    let operator = operator_stack[i].clone();
                    let left = expression_stack[i].clone();
                    let right = new_expression_stack.pop().unwrap();

                    if precedence_level.contains(&operator) {
                        let new_expression =
                            self.construct_binary_expression(left, operator.clone(), right)?;

                        new_expression_stack.push(new_expression);
                    } else {
                        new_expression_stack.push(left);
                        new_expression_stack.push(right);
                        new_operator_stack.push(operator);
                    }
                }
            }

            expression_stack = new_expression_stack;
            operator_stack = new_operator_stack;

            if expression_stack.len() == 1 {
                return Ok(expression_stack[0].clone());
            }
        }

        Err(format!(
            "something is weird in this array: {:?}",
            expression_stack
        ))
    }
    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::If)?;
        self.eat_token(Token::LeftParen)?;

        let test = self.parse_expression()?;

        self.eat_token(Token::RightParen)?;

        let consequent = self.parse_expression()?;

        let alternate = if matches!(self.peek(), Token::Else) {
            self.eat_token(Token::Else)?;
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(Expression::If(If {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate,
        }))
    }
    fn parse_for_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::For)?;
        self.eat_token(Token::LeftParen)?;

        let init = if matches!(self.peek(), Token::SemiColon) {
            None
        } else if matches!(self.peek(), Token::Let | Token::Const) {
            let decl = self.parse_variable_declaration()?;
            Some(Box::new(Statement::VariableDeclaration(decl)))
        } else {
            let expr = self.parse_expression()?;
            Some(Box::new(Statement::Expression(expr)))
        };

        self.eat_token(Token::SemiColon)?;

        let test = if matches!(self.peek(), Token::SemiColon) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.eat_token(Token::SemiColon)?;

        let update = if matches!(self.peek(), Token::RightParen) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        self.eat_token(Token::RightParen)?;

        let body = Box::new(self.parse_expression()?);

        Ok(Expression::For(For {
            init,
            test,
            update,
            body,
        }))
    }
    fn parse_do_while_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::Do)?;

        let body = Box::new(self.parse_expression()?);

        self.eat_token(Token::While)?;
        self.eat_token(Token::LeftParen)?;

        let test = Box::new(self.parse_expression()?);

        self.eat_token(Token::RightParen)?;

        Ok(Expression::DoWhile(DoWhile { test, body }))
    }
    fn parse_while_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::While)?;
        self.eat_token(Token::LeftParen)?;

        let test = Box::new(self.parse_expression()?);

        self.eat_token(Token::RightParen)?;

        let body = Box::new(self.parse_expression()?);

        Ok(Expression::While(While { test, body }))
    }
    fn parse_sequence_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::LeftBrace)?;

        let mut expressions = Vec::new();
        loop {
            expressions.push(self.parse_expression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eat_token(Token::RightBrace)?;

        Ok(Expression::Sequence(Sequence { expressions }))
    }
    fn parse_block_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::LeftBrace)?;

        let mut body = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace => break,
                _ => body.push(self.parse_statement()?),
            }
        }

        self.eat_token(Token::RightBrace)?;

        Ok(Expression::Block(Block { body }))
    }
    fn parse_identifier(&mut self) -> Result<String, String> {
        match self.next() {
            Token::Identifier(name) => Ok(name.to_string()),
            token => Err(format!("expected identifier, got {:?}", token)),
        }
    }
    fn parse_number_literal(&mut self) -> Result<Expression, String> {
        if let Token::Number(raw) = self.next() {
            if let Some(i32) = raw.parse::<i32>().ok() {
                return Ok(Expression::Literal(Literal::I32(i32)));
            } else if let Some(u32) = raw.parse::<u32>().ok() {
                return Ok(Expression::Literal(Literal::U32(u32)));
            } else if let Some(i64) = raw.parse::<i64>().ok() {
                return Ok(Expression::Literal(Literal::I64(i64)));
            } else if let Some(u64) = raw.parse::<u64>().ok() {
                return Ok(Expression::Literal(Literal::U64(u64)));
            } else if let Some(f64) = raw.parse::<f64>().ok() {
                return Ok(Expression::Literal(Literal::F64(f64)));
            } else {
                return Err(format!("could not parse number literal: {}", raw));
            }
        } else {
            Err("expected number literal".to_string())
        }
    }
    fn parse_simd_literal(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::LeftBrace)?;

        let mut elements = Vec::new();
        loop {
            if let Token::Number(raw) = self.peek().clone() {
                elements.push(raw);
            } else {
                return Err(format!("expected number literal, got {:?}", self.peek()));
            }

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eat_token(Token::RightBrace)?;

        macro_rules! simdify {
            ($arr:expr, $ttype:ty, $len:expr) => {{
                let mut new_arr: [$ttype; $len] = Default::default();
                for (i, e) in $arr.iter().enumerate() {
                    new_arr[i] = e.parse::<$ttype>().unwrap();
                }
                new_arr
            }};
        }

        match elements.len() {
            2 => {
                if elements.iter().all(|e| e.parse::<f64>().is_ok())
                    && elements.iter().any(|e| {
                        e.contains('.') || (e.parse::<i64>().is_err() && e.parse::<u64>().is_err())
                    })
                {
                    let f64x2 = simdify!(elements, f64, 2);
                    Ok(Expression::Literal(Literal::F64x2(f64x2)))
                } else if elements.iter().all(|e| e.parse::<i64>().is_ok()) {
                    let i64x2 = simdify!(elements, i64, 2);
                    Ok(Expression::Literal(Literal::I64x2(i64x2)))
                } else if elements.iter().all(|e| e.parse::<u64>().is_ok()) {
                    let u64x2 = simdify!(elements, u64, 2);
                    Ok(Expression::Literal(Literal::U64x2(u64x2)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            4 => {
                if elements.iter().all(|e| e.parse::<f32>().is_ok())
                    && elements.iter().any(|e| {
                        e.contains('.') || (e.parse::<i32>().is_err() && e.parse::<u32>().is_err())
                    })
                {
                    let f32x4 = simdify!(elements, f32, 4);
                    Ok(Expression::Literal(Literal::F32x4(f32x4)))
                } else if elements.iter().all(|e| e.parse::<i32>().is_ok()) {
                    let i32x4 = simdify!(elements, i32, 4);
                    Ok(Expression::Literal(Literal::I32x4(i32x4)))
                } else if elements.iter().all(|e| e.parse::<u32>().is_ok()) {
                    let u32x4 = simdify!(elements, u32, 4);
                    Ok(Expression::Literal(Literal::U32x4(u32x4)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            8 => {
                if elements.iter().all(|e| e.parse::<i16>().is_ok()) {
                    let i16x8 = simdify!(elements, i16, 8);
                    Ok(Expression::Literal(Literal::I16x8(i16x8)))
                } else if elements.iter().all(|e| e.parse::<u16>().is_ok()) {
                    let u16x8 = simdify!(elements, u16, 8);
                    Ok(Expression::Literal(Literal::U16x8(u16x8)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            16 => {
                if elements.iter().all(|e| e.parse::<i8>().is_ok()) {
                    let i8x16 = simdify!(elements, i8, 16);
                    Ok(Expression::Literal(Literal::I8x16(i8x16)))
                } else if elements.iter().all(|e| e.parse::<u8>().is_ok()) {
                    let u8x16 = simdify!(elements, u8, 16);
                    Ok(Expression::Literal(Literal::U8x16(u8x16)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            _ => Err("SIMD literal could not fit into any shape".to_string()),
        }
    }
    fn parse_string_literal(&mut self) -> Result<String, String> {
        if let Token::String(raw) = self.next() {
            Ok(raw.to_string())
        } else {
            Err("expected string literal".to_string())
        }
    }
    fn parse_struct_literal(&mut self, type_annotation: Identifier) -> Result<Expression, String> {
        self.eat_token(Token::LeftBrace)?;

        let type_annotation = TypeIdentifier::Identifier {
            is_ref: false,
            name: type_annotation,
        };

        let mut properties = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightBrace) {
                break;
            }

            let key = self.parse_identifier()?;

            properties.push(Property {
                value: if matches!(self.peek(), Token::Colon) {
                    self.eat_token(Token::Colon)?;
                    self.parse_expression()?
                } else {
                    Expression::Identifier(key.clone())
                },
                key,
            });

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eat_token(Token::RightBrace)?;

        Ok(Expression::Object(Object {
            type_annotation,
            properties,
        }))
    }
    fn parse_array_literal(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::LeftBracket)?;

        let mut elements = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightBracket) {
                break;
            }

            elements.push(self.parse_expression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eat_token(Token::RightBracket)?;

        Ok(Expression::Array(Array { elements }))
    }
    fn parse_call_expression(&mut self, caller: Expression) -> Result<Expression, String> {
        self.eat_token(Token::LeftParen)?;

        let mut arguments = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightParen) {
                break;
            }

            arguments.push(self.parse_expression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eat_token(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eat_token(Token::RightParen)?;

        Ok(Expression::Call(Call {
            callee: Box::new(caller),
            arguments,
        }))
    }
    fn parse_member_expression(&mut self, parent: Expression) -> Result<Expression, String> {
        let mut parent = parent;

        // todo: check if this is correct
        loop {
            self.eat_token(Token::Period)?;

            let property = self.parse_identifier()?;

            parent = Expression::Member(Member {
                object: Box::new(parent),
                property,
            });

            if !matches!(self.peek(), Token::Period) {
                break;
            }
        }

        Ok(parent)
    }
    fn parse_index_expression(&mut self, parent: Expression) -> Result<Expression, String> {
        let mut parent = parent;

        loop {
            self.eat_token(Token::LeftBracket)?;

            let index = self.parse_expression()?;

            self.eat_token(Token::RightBracket)?;

            parent = Expression::Index(Index {
                object: Box::new(parent),
                index: Box::new(index),
            });

            if !matches!(self.peek(), Token::LeftBracket) {
                break;
            }
        }

        Ok(parent)
    }
    fn parse_empty_expression(&mut self) -> Result<Expression, String> {
        Ok(Expression::Empty(Empty {}))
    }
    fn parse_continue_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::Continue)?;
        Ok(Expression::Continue(Continue {}))
    }
    fn parse_break_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::Break)?;
        Ok(Expression::Break(Break {}))
    }
    fn parse_unary_expression(&mut self) -> Result<Expression, String> {
        let operator = self.next().clone();
        if !UNARY_OPERATORS.contains(&operator) {
            return Err(format!(
                "unexpected token in unary expression: {:?}",
                operator
            ));
        }

        let argument = Box::new(self.parse_expression()?);

        Ok(Expression::Unary(Unary {
            operator: match operator {
                Token::Not => UnaryOperator::Not,
                Token::BitwiseNot => UnaryOperator::BitwiseNot,
                Token::Plus => UnaryOperator::Plus,
                Token::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            },
            argument,
        }))
    }

    fn parse_type(&mut self) -> Result<TypeIdentifier, String> {
        let is_ref = if matches!(self.peek(), Token::Ref) {
            self.eat_token(Token::Ref)?;
            true
        } else {
            false
        };

        match self.peek() {
            Token::Identifier(_) => {
                let name = self.parse_identifier()?;
                Ok(TypeIdentifier::Identifier { is_ref, name })
            }
            Token::LeftParen => {
                self.eat_token(Token::LeftParen)?;

                let mut parameters = Vec::new();
                let mut unnamed = 0;
                loop {
                    if matches!(self.peek(), Token::RightParen) {
                        break;
                    }

                    let name;
                    if matches!(self.peek(), Token::Identifier(_))
                        && matches!(self.get_token(self.index + 1), Token::Colon)
                    {
                        name = self.parse_identifier()?;
                        self.eat_token(Token::Colon)?;
                    } else {
                        name = format!("$${}", unnamed);
                        unnamed += 1;
                    }

                    parameters.push(TypedParameter {
                        name,
                        type_annotation: self.parse_type()?,
                    });

                    if matches!(self.peek(), Token::Comma) {
                        self.eat_token(Token::Comma)?;
                    } else {
                        break;
                    }
                }
                self.eat_token(Token::RightParen)?;

                self.eat_token(Token::Assignment)?;
                self.eat_token(Token::GreaterThan)?;

                let return_type = Box::new(self.parse_type()?);

                Ok(TypeIdentifier::Function {
                    is_ref,
                    parameters,
                    return_type,
                })
            }
            Token::LeftBracket => {
                self.eat_token(Token::LeftBracket)?;

                let element_type = Box::new(self.parse_type()?);
                let ident = TypeIdentifier::Array {
                    is_ref,
                    element_type,
                    size: if matches!(self.peek(), Token::SemiColon) {
                        self.eat_token(Token::SemiColon)?;
                        if let Token::Number(raw) = self.next() {
                            raw.parse::<u32>().unwrap()
                        } else {
                            return Err("expected number literal in array type".to_string());
                        }
                    } else {
                        0
                    },
                };

                self.eat_token(Token::RightBracket)?;

                Ok(ident)
            }
            token => Err(format!("unexpected token in type: {:?}", token)),
        }
    }
    fn parse_return_expression(&mut self) -> Result<Expression, String> {
        self.eat_token(Token::Return)?;

        let argument = if matches!(self.peek(), Token::SemiColon) {
            None
        } else {
            Some(Box::new(self.parse_expression()?))
        };

        Ok(Expression::Return(Return { argument }))
    }

    fn construct_binary_expression(
        &self,
        left: Expression,
        operator: Token,
        right: Expression,
    ) -> Result<Expression, String> {
        match operator {
            Token::Assignment
            | Token::AssignmentBitshiftLeft
            | Token::AssignmentBitshiftRight
            | Token::AssignmentPlus
            | Token::AssignmentMinus
            | Token::AssignmentRemainder
            | Token::AssignmentStar
            | Token::AssignmentSlash
            | Token::AssignmentBitwiseAnd
            | Token::AssignmentBitwiseOr
            | Token::AssignmentBitwiseXor => Ok(Expression::Assignment(Assignment {
                operator: match operator {
                    Token::Assignment => AssignmentOperator::Base,
                    Token::AssignmentPlus => AssignmentOperator::Plus,
                    Token::AssignmentMinus => AssignmentOperator::Minus,
                    Token::AssignmentStar => AssignmentOperator::Star,
                    Token::AssignmentSlash => AssignmentOperator::Slash,
                    Token::AssignmentRemainder => AssignmentOperator::Remainder,
                    Token::AssignmentBitwiseAnd => AssignmentOperator::BitwiseAnd,
                    Token::AssignmentBitwiseOr => AssignmentOperator::BitwiseOr,
                    Token::AssignmentBitwiseXor => AssignmentOperator::BitwiseXor,
                    Token::AssignmentBitshiftLeft => AssignmentOperator::BitshiftLeft,
                    Token::AssignmentBitshiftRight => AssignmentOperator::BitshiftRight,
                    _ => unreachable!(),
                },
                left: Box::new(left),
                right: Box::new(right),
            })),
            Token::Plus
            | Token::Minus
            | Token::Star
            | Token::Slash
            | Token::Remainder
            | Token::Equals
            | Token::NotEquals
            | Token::LessThan
            | Token::LessThanEquals
            | Token::GreaterThan
            | Token::GreaterThanEquals
            | Token::BitwiseAnd
            | Token::BitwiseOr
            | Token::BitwiseXor
            | Token::BitshiftLeft
            | Token::BitshiftRight => Ok(Expression::Binary(Binary {
                operator: match operator {
                    Token::Equals => BinaryOperator::Equals,
                    Token::NotEquals => BinaryOperator::NotEquals,
                    Token::LessThan => BinaryOperator::LessThan,
                    Token::LessThanEquals => BinaryOperator::LessThanEquals,
                    Token::GreaterThan => BinaryOperator::GreaterThan,
                    Token::GreaterThanEquals => BinaryOperator::GreaterThanEquals,
                    Token::Plus => BinaryOperator::Plus,
                    Token::Minus => BinaryOperator::Minus,
                    Token::Star => BinaryOperator::Star,
                    Token::Slash => BinaryOperator::Slash,
                    Token::Remainder => BinaryOperator::Remainder,
                    Token::BitwiseAnd => BinaryOperator::And,
                    Token::BitwiseOr => BinaryOperator::Or,
                    Token::BitwiseXor => BinaryOperator::Xor,
                    Token::BitshiftLeft => BinaryOperator::BitshiftLeft,
                    Token::BitshiftRight => BinaryOperator::BitshiftRight,
                    _ => unreachable!(),
                },
                left: Box::new(left),
                right: Box::new(right),
            })),
            Token::LogicalAnd | Token::LogicalOr => Ok(Expression::Logical(Logical {
                operator: match operator {
                    Token::LogicalAnd => LogicalOperator::And,
                    Token::LogicalOr => LogicalOperator::Or,
                    _ => unreachable!(),
                },
                left: Box::new(left),
                right: Box::new(right),
            })),
            token => Err(format!(
                "unexpected token in binary expression: {:?}",
                token
            )),
        }
    }

    fn get_token(&self, idx: usize) -> &Token {
        self.tokens.get(idx).unwrap_or(&Token::EOF)
    }

    fn expect_token(&self, token: Token) -> Result<(), String> {
        if *self.peek() == token {
            Ok(())
        } else {
            Err(format!("expected token {:?}, got {:?}", token, self.peek()))
        }
    }

    fn eat_token(&mut self, token: Token) -> Result<(), String> {
        self.expect_token(token)?;
        self.next();
        Ok(())
    }

    fn peek(&self) -> &Token {
        self.get_token(self.index)
    }

    fn next(&mut self) -> &Token {
        self.index += 1;
        self.get_token(self.index - 1)
    }
}
