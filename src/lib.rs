// 声明子模块
pub mod expr;
pub mod item;
pub mod stmt;
pub mod ty;
pub mod use_decl;
pub mod ast_printer;

// 重新导出所有子模块的公共内容，方便外部使用
pub use expr::*;
pub use item::*;
pub use stmt::*;
pub use ty::*;
pub use use_decl::*;

/// 代表一个被解析的源文件（一个模块）
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
}

use std::marker::PhantomData;
use nyanc_core::tokens::Token;

// --- 1. 定义类型安全的 AST 节点 ID ---
/// 代表一个 AST 节点在“仓库”中的唯一ID。
/// 它是泛型的，`AstId<Expr>` 和 `AstId<Stmt>` 是不同的类型，防止误用。
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstId<N> {
    raw: u32,
    _marker: PhantomData<fn() -> N>, // 用于让编译器区分不同的 AstId<T>
}

// --- 2. 定义 AST “中央仓库” (Arena) ---
/// 这个结构体将拥有我们解析出的所有 AST 节点
#[derive(Debug, Default)]
pub struct Ast {
    // 每个货架存放一种类型的节点
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
    pub exprs: Vec<Expr>,
    pub types: Vec<Type>,
    pub params: Vec<Param>, // 参数/字段也需要入库
}

// --- 3. 为仓库添加方便的“入库”方法 ---
impl Ast {
    pub fn new() -> Self {
        Self::default()
    }

    // --- Expression ---
    pub fn alloc_expr(&mut self, expr: Expr) -> AstId<Expr> {
        let index = self.exprs.len() as u32;
        self.exprs.push(expr);
        AstId { raw: index, _marker: PhantomData }
    }

    // --- Statement ---
    pub fn alloc_stmt(&mut self, stmt: Stmt) -> AstId<Stmt> {
        let index = self.stmts.len() as u32;
        self.stmts.push(stmt);
        AstId { raw: index, _marker: PhantomData }
    }

    // --- Item ---
    pub fn alloc_item(&mut self, item: Item) -> AstId<Item> {
        let index = self.items.len() as u32;
        self.items.push(item);
        AstId { raw: index, _marker: PhantomData }
    }

    // --- Type ---
    pub fn alloc_type(&mut self, ty: Type) -> AstId<Type> {
        let index = self.types.len() as u32;
        self.types.push(ty);
        AstId { raw: index, _marker: PhantomData }
    }

    // --- Param ---
    pub fn alloc_param(&mut self, param: Param) -> AstId<Param> {
        let index = self.params.len() as u32;
        self.params.push(param);
        AstId { raw: index, _marker: PhantomData }
    }
}