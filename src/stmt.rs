use super::{Expr, Type, AstId};
use nyanc_core::tokens::Token;

/// 语句的枚举
#[derive(Debug, Clone)]
pub enum Stmt {
    // let a = 1;
    Let(LetStmt),
    // a: int = 1;
    Var(VarStmt),
    // return a + 1;
    Return(ReturnStmt),
    // { ... }
    Block(AstId<BlockStmt>),
    // a = a + 1; or my_func();
    Expression(ExprStmt),
    // if condition { ... } else { ... }
    If(IfStmt),       
    // while condition { ... }
    While(WhileStmt), 
}

/// `let a = 1;`
#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: Token,
    pub value: AstId<Expr>,
}

/// `a: int = 1;`
#[derive(Debug, Clone)]
pub struct VarStmt {
    pub name: Token,
    pub var_type: AstId<Type>,
    pub value: Option<AstId<Expr>>, // 初始值是可选的
}

/// `return a + 1;`
#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: Token, // "return" 关键字本身，用于错误报告
    pub value: Option<AstId<Expr>>, // 允许 `return;`
}

/// `{ ... }`
#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub stmts: Vec<AstId<Stmt>>,
}

/// `a = a + 1;` or `my_func();`
#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expr: AstId<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: AstId<Expr>,
    pub then_branch: AstId<BlockStmt>,
    /// else 分支是可选的，并且可以是另一个 if 语句（用于 else if）或一个代码块
    pub else_branch: Option<AstId<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: AstId<Expr>,
    pub body: AstId<BlockStmt>,
}
