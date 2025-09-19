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
    pub items: Vec<AstId<Item>>,
}

use std::marker::PhantomData;

// --- 1. 定义类型安全的 AST 节点 ID ---
/// 代表一个 AST 节点在“仓库”中的唯一ID。
/// 它是泛型的，`AstId<Expr>` 和 `AstId<Stmt>` 是不同的类型，防止误用。
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct AstId<N> {
    raw: u32,
    _marker: PhantomData<fn() -> N>, // 用于让编译器区分不同的 AstId<T>
}
// --- 核心修复点：手动实现 Clone 和 Copy ---
// 手动实现这些 Trait，从而告诉编译器：
// “无论泛型 N 是什么，AstId 本身都可以被安全地、廉价地复制。”
impl<N> Clone for AstId<N> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<N> Copy for AstId<N> {}

impl<N> AstId<N> {
    pub fn get_raw(self) -> u32 {
        return self.raw;
    }
}

// --- 2. 定义 AST “中央仓库” (Arena) ---
/// 这个结构体将拥有解析出的所有 AST 节点
#[derive(Debug, Default)]
pub struct Ast {
    // 每个货架存放一种类型的节点
    pub modules: Vec<Module>, 
    pub items: Vec<Item>,
    pub stmts: Vec<Stmt>,
    pub block_stmts: Vec<BlockStmt>,
    pub exprs: Vec<Expr>,
    pub types: Vec<Type>,
    pub params: Vec<Param>, 
    pub paths: Vec<Path>,         
    pub use_trees: Vec<UseTree>, 
}

// --- 3. 为仓库添加方便的“入库”方法 ---
impl Ast {
    pub fn new() -> Self {
        Self::default()
    }

    // --- Module  ---
    pub fn alloc_module(&mut self, module: Module) -> AstId<Module> {
        let index = self.modules.len() as u32;
        self.modules.push(module);
        AstId { raw: index, _marker: PhantomData }
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

    // --- BlockStmt ---
    pub fn alloc_block_stmt(&mut self, block_stmt: BlockStmt) -> AstId<BlockStmt> {
        let index = self.block_stmts.len() as u32;
        self.block_stmts.push(block_stmt);
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

    // --- Path ---
    pub fn alloc_path(&mut self, path: Path) -> AstId<Path> {
        let index = self.paths.len() as u32;
        self.paths.push(path);
        AstId { raw: index, _marker: PhantomData }
    }

    // --- UseTree ---
    pub fn alloc_use_tree(&mut self, tree: UseTree) -> AstId<UseTree> {
        let index = self.use_trees.len() as u32;
        self.use_trees.push(tree);
        AstId { raw: index, _marker: PhantomData }
    }
}