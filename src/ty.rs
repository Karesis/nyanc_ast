use nyanc_core::tokens::Token;
use super::AstId;

#[derive(Debug, Clone)]
pub enum Type {
    /// `int`, `bool`, `Point`
    Identifier { name: Token },
    /// `^int`, `^Point`
    Pointer { base: AstId<Type> },
    /// `()` - 单元类型
    Unit, 
}