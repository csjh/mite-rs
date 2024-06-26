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

#[derive(Clone, PartialEq, Debug)]
enum TypeIdentifier {
    Identifier {
        is_ref: bool,
        name: String,
    },
    Array {
        is_ref: bool,
        element_type: Box<TypeIdentifier>,
        size: usize,
    },
    Function {
        is_ref: bool,
        parameters: Vec<TypedParameter>,
        return_type: Box<TypeIdentifier>,
    },
}

type ProgramBody = Declaration;

#[derive(Clone, PartialEq, Debug)]
pub(crate) struct Program {
    body: Vec<ProgramBody>,
}

#[derive(Clone, PartialEq, Debug)]
struct TypedParameter {
    name: Identifier,
    typeAnnotation: TypeIdentifier,
}

#[derive(Clone, PartialEq, Debug)]
struct FunctionDeclaration {
    name: Identifier,
    parameters: Vec<TypedParameter>,
    return_type: TypeIdentifier,
    body: Expression,
}

#[derive(Clone, PartialEq, Debug)]
struct Property {
    key: Identifier,
    value: Expression,
}

#[derive(Clone, PartialEq, Debug)]
struct VariableDeclarator {
    id: Identifier,
    init: Option<Expression>,
    typeAnnotation: Option<TypeIdentifier>,
}

#[derive(Clone, PartialEq, Debug)]
enum Literal {
    string(String),
    // f32(f32),
    f64(f64),
    i32(i32),
    u32(u32),
    i64(i64),
    u64(u64),
    // bool(bool),
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

#[derive(Clone, PartialEq, Debug)]
struct StructField {
    name: Identifier,
    typeAnnotation: TypeIdentifier,
}

#[derive(Clone, PartialEq, Debug)]
struct ImportSpecifier {
    local: Identifier,
    imported: Identifier,
    typeAnnotation: Option<TypeIdentifier>,
}

#[derive(Clone, PartialEq, Debug)]
enum VariableKind {
    Let,
    Const,
}

#[derive(Clone, PartialEq, Debug)]
enum Declaration {
    FunctionDeclaration(FunctionDeclaration),
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

#[derive(Clone, PartialEq, Debug)]
enum Statement {
    ExpressionStatement { expression: Expression },
    ReturnStatement { argument: Option<Expression> },
    Declaration(Declaration),
}

#[derive(Clone, PartialEq, Debug)]
enum ForExpressionInit {
    VariableDeclaration(Declaration),
    Expression(Expression),
}

#[derive(Clone, PartialEq, Debug)]
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

pub fn parse(tokens: Vec<Token>) -> Program {
    Parser::parse(tokens)
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
                    let result = self.parseTopLevelDeclaration();
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

        self.expectToken(Token::EOF).unwrap();

        Program { body }
    }

    fn parseTopLevelDeclaration(&mut self) -> Result<ProgramBody, String> {
        match self.peek() {
            Token::Fn => Ok(Declaration::FunctionDeclaration(self.parseFunction(false)?)),
            Token::Struct => self.parseStruct(),
            Token::Export => self.parseExport(),
            Token::Import => self.parseImport(),
            Token::Let | Token::Const => {
                let decl = self.parseVariableDeclaration();
                self.eatToken(Token::SemiColon)?;
                decl
            }
            token => Err(format!("unexpected token at top level: {:?}", token)),
        }
    }
    fn parseImport(&mut self) -> Result<ProgramBody, String> {
        self.eatToken(Token::Import)?;
        self.eatToken(Token::LeftBrace)?;

        let mut specifiers = Vec::new();
        loop {
            let imported = self.parseIdentifier()?;

            let local = if matches!(self.peek(), Token::As) {
                self.eatToken(Token::As)?;
                self.parseIdentifier()?
            } else {
                imported.clone()
            };

            let specifier = ImportSpecifier {
                local,
                imported,
                typeAnnotation: if matches!(self.peek(), Token::Colon) {
                    self.eatToken(Token::Colon)?;
                    Some(self.parseType()?)
                } else {
                    None
                },
            };

            specifiers.push(specifier);

            if matches!(self.peek(), Token::RightBrace) {
                break;
            }

            self.eatToken(Token::Comma)?;
        }

        self.eatToken(Token::RightBrace)?;
        self.eatToken(Token::From)?;

        let source = self.parseStringLiteral()?;

        self.eatToken(Token::SemiColon)?;

        Ok(ProgramBody::ImportDeclaration { source, specifiers })
    }
    fn parseExport(&mut self) -> Result<ProgramBody, String> {
        self.eatToken(Token::EOF)?;

        match self.parseTopLevelDeclaration()? {
            ProgramBody::ExportNamedDeclaration { .. } | ProgramBody::ImportDeclaration { .. } => {
                Err("export/import cannot be nested".to_string())
            }
            decl => Ok(ProgramBody::ExportNamedDeclaration {
                declaration: Box::new(decl),
            }),
        }
    }
    fn parseStruct(&mut self) -> Result<ProgramBody, String> {
        self.eatToken(Token::Struct)?;

        let id = self.parseIdentifier()?;

        self.eatToken(Token::LeftBrace)?;

        let mut fields = Vec::new();
        let mut methods = Vec::new();

        loop {
            match self.peek() {
                Token::RightBrace => break,
                Token::Identifier(_) => match self.getToken(self.index + 1) {
                    Token::Colon => {
                        let name = self.parseIdentifier()?;
                        self.eatToken(Token::Colon)?;
                        let typeAnnotation = self.parseType()?;

                        fields.push(StructField {
                            name,
                            typeAnnotation,
                        });
                    }
                    Token::LeftParen => {
                        let mut func = self.parseFunction(true)?;
                        func.parameters.insert(
                            0,
                            TypedParameter {
                                name: "this".to_string(),
                                typeAnnotation: TypeIdentifier::Identifier {
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

        self.eatToken(Token::RightBrace)?;

        Ok(ProgramBody::StructDeclaration {
            id,
            fields,
            methods,
        })
    }
    fn parseFunction(&mut self, is_method: bool) -> Result<FunctionDeclaration, String> {
        if !is_method {
            self.eatToken(Token::Fn)?;
        }

        let name = self.parseIdentifier()?;

        self.eatToken(Token::LeftParen)?;

        let mut parameters = Vec::new();
        loop {
            match self.peek() {
                Token::RightParen => break,
                Token::Comma => {
                    self.eatToken(Token::Comma)?;
                }
                _ => {
                    let name = self.parseIdentifier()?;
                    self.eatToken(Token::Colon)?;
                    let typeAnnotation = self.parseType()?;

                    parameters.push(TypedParameter {
                        name,
                        typeAnnotation,
                    });
                }
            }
        }

        self.eatToken(Token::RightParen)?;
        self.eatToken(Token::Colon)?;

        let return_type = self.parseType()?;
        let body = self.parseBlockExpression()?;

        Ok(FunctionDeclaration {
            name,
            parameters,
            return_type,
            body,
        })
    }
    fn parseVariableDeclaration(&mut self) -> Result<ProgramBody, String> {
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

        self.eatToken(self.peek().clone())?;

        let mut declarations = Vec::new();
        loop {
            let id = self.parseIdentifier()?;

            let typeAnnotation = if matches!(self.peek(), Token::Colon) {
                self.eatToken(Token::Colon)?;
                Some(self.parseType()?)
            } else {
                None
            };

            let init = if matches!(self.peek(), Token::Assignment) {
                self.eatToken(Token::Assignment)?;
                Some(self.parseExpression()?)
            } else {
                None
            };

            if init.is_none() && typeAnnotation.is_none() {
                return Err("variable declaration must have a type or an initializer".to_string());
            }

            declarations.push(VariableDeclarator {
                id,
                init,
                typeAnnotation,
            });

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        Ok(ProgramBody::VariableDeclaration { declarations, kind })
    }

    fn parseStatement(&mut self) -> Result<Statement, String> {
        let statement;

        match self.peek() {
            Token::Return => {
                self.eatToken(Token::Return)?;
                statement = Statement::ReturnStatement {
                    argument: Some(self.parseExpression()?),
                };
            }
            Token::Let | Token::Const => {
                statement = Statement::Declaration(self.parseVariableDeclaration()?);
            }
            _ => {
                statement = Statement::ExpressionStatement {
                    expression: self.parseExpression()?,
                };
            }
        }

        self.eatToken(Token::SemiColon)?;

        Ok(statement)
    }

    fn parseExpression(&mut self) -> Result<Expression, String> {
        let mut expression_stack = Vec::new();
        let mut operator_stack = Vec::new();

        if matches!(self.peek(), Token::Return | Token::Let | Token::Const) {
            self.index -= 1;
            return Ok(Expression::BlockExpression {
                body: vec![self.parseStatement()?],
            });
        }

        loop {
            match self.peek() {
                Token::SemiColon | Token::RightParen => {
                    expression_stack.push(self.parseEmptyExpression()?)
                }
                Token::LeftBracket => expression_stack.push(self.parseArrayLiteral()?),
                Token::If => expression_stack.push(self.parseIfExpression()?),
                Token::LeftBrace => {
                    if let Token::Number(_) = *self.getToken(self.index + 1) {
                        expression_stack.push(self.parseSIMDLiteral()?);
                    } else {
                        expression_stack.push(self.parseBlockExpression()?);
                    }
                }
                Token::LeftParen => expression_stack.push(self.parseSequenceExpression()?),
                Token::For => expression_stack.push(self.parseForExpression()?),
                Token::Do => expression_stack.push(self.parseDoWhileExpression()?),
                Token::While => expression_stack.push(self.parseWhileExpression()?),
                Token::Continue => expression_stack.push(self.parseContinueExpression()?),
                Token::Break => expression_stack.push(self.parseBreakExpression()?),
                Token::Number(_) => expression_stack.push(self.parseNumberLiteral()?),
                Token::String(_) => {
                    expression_stack.push(Expression::Literal(self.parseStringLiteral()?))
                }
                Token::Identifier(_) => {
                    let next = self.parseIdentifier()?;
                    if *self.getToken(self.index + 1) == Token::LeftBrace {
                        expression_stack.push(self.parseStructLiteral(next)?);
                    } else {
                        expression_stack.push(Expression::Identifier(next));
                    }
                }
                Token::Not | Token::BitwiseNot | Token::Plus | Token::Minus => {
                    expression_stack.push(self.parseUnaryExpression()?);
                }
                _ => (),
            }

            loop {
                let last = expression_stack.pop().unwrap();
                match self.peek() {
                    Token::LeftBracket => expression_stack.push(self.parseIndexExpression(last)?),
                    Token::Period => expression_stack.push(self.parseMemberExpression(last)?),
                    Token::LeftParen => expression_stack.push(self.parseCallExpression(last)?),
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

        for (j, precedence_level) in precedence.iter().enumerate() {
            let RIGHT_TO_LEFT = j == precedence.len() - 1;

            let mut new_expression_stack = Vec::new();
            // probably not needed
            // if expression_stack.len() == 0 {
            //    expression_stack.push(Expression::EmptyExpression);
            // }
            let mut new_operator_stack = Vec::new();

            if !RIGHT_TO_LEFT {
                new_expression_stack.push(expression_stack.first().unwrap().clone());
                for i in 0..operator_stack.len() {
                    let operator = operator_stack[i].clone();
                    let left = new_expression_stack.pop().unwrap();
                    let right = expression_stack[i + 1].clone();

                    if precedence_level.contains(&operator) {
                        let new_expression =
                            self.constructBinaryExpression(left, operator.clone(), right)?;

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
                            self.constructBinaryExpression(left, operator.clone(), right)?;

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
    fn parseIfExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::If)?;
        self.eatToken(Token::LeftParen)?;

        let test = self.parseExpression()?;

        self.eatToken(Token::RightParen)?;

        let consequent = self.parseExpression()?;

        let alternate = if matches!(self.peek(), Token::Else) {
            self.eatToken(Token::Else)?;
            Some(Box::new(self.parseExpression()?))
        } else {
            None
        };

        Ok(Expression::IfExpression {
            test: Box::new(test),
            consequent: Box::new(consequent),
            alternate,
        })
    }
    fn parseForExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::For)?;
        self.eatToken(Token::LeftParen)?;

        let init = if matches!(self.peek(), Token::SemiColon) {
            None
        } else if matches!(self.peek(), Token::Let | Token::Const) {
            let decl = self.parseVariableDeclaration()?;
            Some(Box::new(ForExpressionInit::VariableDeclaration(decl)))
        } else {
            let expr = self.parseExpression()?;
            Some(Box::new(ForExpressionInit::Expression(expr)))
        };

        self.eatToken(Token::SemiColon)?;

        let test = if matches!(self.peek(), Token::SemiColon) {
            None
        } else {
            Some(Box::new(self.parseExpression()?))
        };

        self.eatToken(Token::SemiColon)?;

        let update = if matches!(self.peek(), Token::RightParen) {
            None
        } else {
            Some(Box::new(self.parseExpression()?))
        };

        self.eatToken(Token::RightParen)?;

        let body = Box::new(self.parseExpression()?);

        Ok(Expression::ForExpression {
            init,
            test,
            update,
            body,
        })
    }
    fn parseDoWhileExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::Do)?;

        let body = Box::new(self.parseExpression()?);

        self.eatToken(Token::While)?;
        self.eatToken(Token::LeftParen)?;

        let test = Box::new(self.parseExpression()?);

        self.eatToken(Token::RightParen)?;

        Ok(Expression::DoWhileExpression { test, body })
    }
    fn parseWhileExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::While)?;
        self.eatToken(Token::LeftParen)?;

        let test = Box::new(self.parseExpression()?);

        self.eatToken(Token::RightParen)?;

        let body = Box::new(self.parseExpression()?);

        Ok(Expression::WhileExpression { test, body })
    }
    fn parseSequenceExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::LeftBrace)?;

        let mut expressions = Vec::new();
        loop {
            expressions.push(self.parseExpression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eatToken(Token::RightBrace)?;

        Ok(Expression::SequenceExpression { expressions })
    }
    fn parseBlockExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::LeftBrace)?;

        let mut body = Vec::new();
        loop {
            match self.peek() {
                Token::RightBrace => break,
                _ => body.push(self.parseStatement()?),
            }
        }

        self.eatToken(Token::RightBrace)?;

        Ok(Expression::BlockExpression { body })
    }
    fn parseIdentifier(&mut self) -> Result<String, String> {
        match self.next() {
            Token::Identifier(name) => Ok(name.to_string()),
            token => Err(format!("expected identifier, got {:?}", token)),
        }
    }
    fn parseNumberLiteral(&mut self) -> Result<Expression, String> {
        if let Token::Number(raw) = self.next() {
            if let Some(i32) = raw.parse::<i32>().ok() {
                return Ok(Expression::Literal(Literal::i32(i32)));
            } else if let Some(u32) = raw.parse::<u32>().ok() {
                return Ok(Expression::Literal(Literal::u32(u32)));
            } else if let Some(i64) = raw.parse::<i64>().ok() {
                return Ok(Expression::Literal(Literal::i64(i64)));
            } else if let Some(u64) = raw.parse::<u64>().ok() {
                return Ok(Expression::Literal(Literal::u64(u64)));
            } else if let Some(f64) = raw.parse::<f64>().ok() {
                return Ok(Expression::Literal(Literal::f64(f64)));
            } else {
                return Err(format!("could not parse number literal: {}", raw));
            }
        } else {
            Err("expected number literal".to_string())
        }
    }
    fn parseSIMDLiteral(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::LeftBrace)?;

        let mut elements = Vec::new();
        loop {
            if let Token::Number(raw) = self.peek().clone() {
                elements.push(raw);
            } else {
                return Err(format!("expected number literal, got {:?}", self.peek()));
            }

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eatToken(Token::RightBrace)?;

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
                    Ok(Expression::Literal(Literal::f64x2(f64x2)))
                } else if elements.iter().all(|e| e.parse::<i64>().is_ok()) {
                    let i64x2 = simdify!(elements, i64, 2);
                    Ok(Expression::Literal(Literal::i64x2(i64x2)))
                } else if elements.iter().all(|e| e.parse::<u64>().is_ok()) {
                    let u64x2 = simdify!(elements, u64, 2);
                    Ok(Expression::Literal(Literal::u64x2(u64x2)))
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
                    Ok(Expression::Literal(Literal::f32x4(f32x4)))
                } else if elements.iter().all(|e| e.parse::<i32>().is_ok()) {
                    let i32x4 = simdify!(elements, i32, 4);
                    Ok(Expression::Literal(Literal::i32x4(i32x4)))
                } else if elements.iter().all(|e| e.parse::<u32>().is_ok()) {
                    let u32x4 = simdify!(elements, u32, 4);
                    Ok(Expression::Literal(Literal::u32x4(u32x4)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            8 => {
                if elements.iter().all(|e| e.parse::<i16>().is_ok()) {
                    let i16x8 = simdify!(elements, i16, 8);
                    Ok(Expression::Literal(Literal::i16x8(i16x8)))
                } else if elements.iter().all(|e| e.parse::<u16>().is_ok()) {
                    let u16x8 = simdify!(elements, u16, 8);
                    Ok(Expression::Literal(Literal::u16x8(u16x8)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            16 => {
                if elements.iter().all(|e| e.parse::<i8>().is_ok()) {
                    let i8x16 = simdify!(elements, i8, 16);
                    Ok(Expression::Literal(Literal::i8x16(i8x16)))
                } else if elements.iter().all(|e| e.parse::<u8>().is_ok()) {
                    let u8x16 = simdify!(elements, u8, 16);
                    Ok(Expression::Literal(Literal::u8x16(u8x16)))
                } else {
                    Err("SIMD literal could not fit into any shape".to_string())
                }
            }
            _ => Err("SIMD literal could not fit into any shape".to_string()),
        }
    }
    fn parseStringLiteral(&mut self) -> Result<Literal, String> {
        if let Token::String(raw) = self.next() {
            Ok(Literal::string(raw.to_string()))
        } else {
            Err("expected string literal".to_string())
        }
    }
    fn parseStructLiteral(&mut self, typeAnnotation: Identifier) -> Result<Expression, String> {
        self.eatToken(Token::LeftBrace)?;

        let typeAnnotation = TypeIdentifier::Identifier {
            is_ref: false,
            name: typeAnnotation,
        };

        let mut properties = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightBrace) {
                break;
            }

            let key = self.parseIdentifier()?;

            properties.push(Property {
                value: if matches!(self.peek(), Token::Colon) {
                    self.eatToken(Token::Colon)?;
                    self.parseExpression()?
                } else {
                    Expression::Identifier(key.clone())
                },
                key,
            });

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eatToken(Token::RightBrace)?;

        Ok(Expression::ObjectExpression {
            typeAnnotation,
            properties,
        })
    }
    fn parseArrayLiteral(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::LeftBracket)?;

        let mut elements = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightBracket) {
                break;
            }

            elements.push(self.parseExpression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eatToken(Token::RightBracket)?;

        Ok(Expression::ArrayExpression { elements })
    }
    fn parseCallExpression(&mut self, caller: Expression) -> Result<Expression, String> {
        self.eatToken(Token::LeftParen)?;

        let mut arguments = Vec::new();
        loop {
            if matches!(self.peek(), Token::RightParen) {
                break;
            }

            arguments.push(self.parseExpression()?);

            if matches!(self.peek(), Token::Comma) {
                self.eatToken(Token::Comma)?;
            } else {
                break;
            }
        }

        self.eatToken(Token::RightParen)?;

        Ok(Expression::CallExpression {
            callee: Box::new(caller),
            arguments,
        })
    }
    fn parseMemberExpression(&mut self, parent: Expression) -> Result<Expression, String> {
        let mut parent = parent;

        // todo: check if this is correct
        loop {
            self.eatToken(Token::Period)?;

            let property = self.parseIdentifier()?;

            parent = Expression::MemberExpression {
                object: Box::new(parent),
                property,
            };

            if !matches!(self.peek(), Token::Period) {
                break;
            }
        }

        Ok(parent)
    }
    fn parseIndexExpression(&mut self, parent: Expression) -> Result<Expression, String> {
        let mut parent = parent;

        loop {
            self.eatToken(Token::LeftBracket)?;

            let index = self.parseExpression()?;

            self.eatToken(Token::RightBracket)?;

            parent = Expression::IndexExpression {
                object: Box::new(parent),
                index: Box::new(index),
            };

            if !matches!(self.peek(), Token::LeftBracket) {
                break;
            }
        }

        Ok(parent)
    }
    fn parseEmptyExpression(&mut self) -> Result<Expression, String> {
        Ok(Expression::EmptyExpression)
    }
    fn parseContinueExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::Continue)?;
        Ok(Expression::ContinueExpression)
    }
    fn parseBreakExpression(&mut self) -> Result<Expression, String> {
        self.eatToken(Token::Break)?;
        Ok(Expression::BreakExpression)
    }
    fn parseUnaryExpression(&mut self) -> Result<Expression, String> {
        let operator = self.next().clone();
        if !unary_operators.contains(&operator) {
            return Err(format!(
                "unexpected token in unary expression: {:?}",
                operator
            ));
        }

        let argument = Box::new(self.parseExpression()?);

        Ok(Expression::UnaryExpression { operator, argument })
    }

    fn parseType(&mut self) -> Result<TypeIdentifier, String> {
        let is_ref = if matches!(self.peek(), Token::Ref) {
            self.eatToken(Token::Ref)?;
            true
        } else {
            false
        };

        match self.peek() {
            Token::Identifier(_) => {
                let name = self.parseIdentifier()?;
                Ok(TypeIdentifier::Identifier { is_ref, name })
            }
            Token::LeftParen => {
                self.eatToken(Token::LeftParen)?;

                let mut parameters = Vec::new();
                let mut unnamed = 0;
                loop {
                    if matches!(self.peek(), Token::RightParen) {
                        break;
                    }

                    let name;
                    if matches!(self.peek(), Token::Identifier(_))
                        && matches!(self.getToken(self.index + 1), Token::Colon)
                    {
                        name = self.parseIdentifier()?;
                        self.eatToken(Token::Colon)?;
                    } else {
                        name = format!("$${}", unnamed);
                        unnamed += 1;
                    }

                    parameters.push(TypedParameter {
                        name,
                        typeAnnotation: self.parseType()?,
                    });

                    if matches!(self.peek(), Token::Comma) {
                        self.eatToken(Token::Comma)?;
                    } else {
                        break;
                    }
                }
                self.eatToken(Token::RightParen)?;

                self.eatToken(Token::Assignment)?;
                self.eatToken(Token::GreaterThan)?;

                let return_type = Box::new(self.parseType()?);

                Ok(TypeIdentifier::Function {
                    is_ref,
                    parameters,
                    return_type,
                })
            }
            Token::LeftBracket => {
                self.eatToken(Token::LeftBracket)?;

                let element_type = Box::new(self.parseType()?);
                let ident = TypeIdentifier::Array {
                    is_ref,
                    element_type,
                    size: if matches!(self.peek(), Token::SemiColon) {
                        self.eatToken(Token::SemiColon)?;
                        if let Token::Number(raw) = self.next() {
                            raw.parse::<usize>().unwrap()
                        } else {
                            return Err("expected number literal in array type".to_string());
                        }
                    } else {
                        0
                    },
                };

                self.eatToken(Token::RightBracket)?;

                Ok(ident)
            }
            token => Err(format!("unexpected token in type: {:?}", token)),
        }
    }

    fn constructBinaryExpression(
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
            | Token::AssignmentBitwiseXor => Ok(Expression::AssignmentExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
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
            | Token::BitshiftRight => Ok(Expression::BinaryExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
            Token::LogicalAnd | Token::LogicalOr => Ok(Expression::LogicalExpression {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            }),
            token => Err(format!(
                "unexpected token in binary expression: {:?}",
                token
            )),
        }
    }

    fn getToken(&self, idx: usize) -> &Token {
        self.tokens.get(idx).unwrap_or(&Token::EOF)
    }

    fn expectToken(&self, token: Token) -> Result<(), String> {
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

    fn peek(&self) -> &Token {
        self.getToken(self.index)
    }

    fn next(&mut self) -> &Token {
        self.index += 1;
        self.getToken(self.index - 1)
    }
}
