// ast/src/ast_printer.rs

use crate::*; // Import all AST node definitions from the current crate
use nyanc_core::tokens::Token;

/// Responsible for serializing AST nodes into an S-expression style string for snapshot testing.
/// It needs a reference to the `Ast` arena to look up nodes by their `AstId`.
pub struct AstPrinter<'ast> {
    ast: &'ast Ast,
}

impl<'ast> AstPrinter<'ast> {
    /// Creates a new AstPrinter instance.
    pub fn new(ast: &'ast Ast) -> Self {
        Self { ast }
    }

    /// Entry point for printing an entire module.
    pub fn print_module(&self, module: &Module) -> String {
        let parts: Vec<String> = module.items.iter().map(|&item_id| self.print_item(item_id)).collect();
        self.parenthesize("module", &parts)
    }

    // --- Item Printers ---

    fn print_item(&self, item_id: AstId<Item>) -> String {
        let item = &self.ast.items[item_id.raw as usize]; // Look up the node by its ID
        match item {
            Item::Use(stmt) => self.print_use_statement(stmt),
            Item::Function(def) => self.print_function_definition(def),
            Item::Struct(def) => self.print_struct_definition(def),
        }
    }

    fn print_use_statement(&self, stmt: &UseStmt) -> String {
        let tree_str = self.print_use_tree(stmt.tree);
        if let Some(prefix_id) = stmt.prefix {
            let prefix_str = self.print_path(prefix_id);
            self.parenthesize("use", &[format!("{}::{}", prefix_str, tree_str)])
        } else {
            self.parenthesize("use", &[tree_str])
        }
    }

    fn print_use_tree(&self, tree_id: AstId<UseTree>) -> String {
        let tree = &self.ast.use_trees[tree_id.raw as usize];
        match tree {
            UseTree::Simple { path, alias } => {
                let path_str = self.print_path(*path);
                if let Some(alias) = alias {
                    format!("(as {} {})", path_str, &alias.lexeme)
                } else {
                    path_str
                }
            }
            UseTree::Group { items } => {
                let item_strs: Vec<String> = items.iter().map(|&item_id| self.print_use_tree(item_id)).collect();
                format!("{{{}}}", item_strs.join(" "))
            }
            UseTree::Wildcard { .. } => "*".to_string(),
        }
    }

    fn print_path(&self, path_id: AstId<Path>) -> String {
        let path = &self.ast.paths[path_id.raw as usize];
        path.segments
            .iter()
            .map(|tok| &*tok.lexeme.as_str())
            .collect::<Vec<&str>>()
            .join("::")
    }

    fn print_function_definition(&self, def: &FunctionDef) -> String {
        let params_str = if def.params.is_empty() {
            "()".to_string()
        } else {
            def.params.iter().map(|&param_id| self.print_param(param_id)).collect::<Vec<_>>().join(" ")
        };
        
        let mut parts = vec![self.parenthesize("params", &[params_str])];

        if let Some(ret_type_id) = def.return_type {
            parts.push(self.parenthesize("returns", &[self.print_type(ret_type_id)]));
        }

        parts.push(self.print_block_statement(def.body));
        self.parenthesize(&format!("fun {}", &def.name.lexeme), &parts)
    }
    
    fn print_struct_definition(&self, def: &StructDef) -> String {
        let fields_str = if def.fields.is_empty() {
            "()".to_string()
        } else {
            def.fields.iter().map(|&field_id| self.print_param(field_id)).collect::<Vec<_>>().join(" ")
        };
        
        let parts = vec![self.parenthesize("fields", &[fields_str])];
        self.parenthesize(&format!("struct {}", &def.name.lexeme), &parts)
    }

    fn print_param(&self, param_id: AstId<Param>) -> String {
        let param = &self.ast.params[param_id.raw as usize];
        format!("({}: {})", &param.name.lexeme, self.print_type(param.param_type))
    }

    // --- Statement Printers ---

    fn print_statement(&self, stmt_id: AstId<Stmt>) -> String {
        let stmt = &self.ast.stmts[stmt_id.raw as usize];
        match stmt {
            Stmt::Let(s) => self.parenthesize(&format!("let {}", &s.name.lexeme), &[self.print_expression(s.value)]),
            Stmt::Var(s) => {
                let type_str = self.print_type(s.var_type);
                if let Some(val_id) = s.value {
                    self.parenthesize(&format!("var {}: {}", &s.name.lexeme, type_str), &[self.print_expression(val_id)])
                } else {
                    format!("(var {}: {})", &s.name.lexeme, type_str)
                }
            }
            Stmt::Return(s) => match s.value {
                Some(val_id) => self.parenthesize("return", &[self.print_expression(val_id)]),
                None => "(return)".to_string(),
            },
            Stmt::Block(block_id) => self.print_block_statement(*block_id),
            Stmt::Expression(s) => self.parenthesize("expr_stmt", &[self.print_expression(s.expr)]),
            Stmt::If(s) => {
                let cond = self.print_expression(s.condition);
                let then = self.print_block_statement(s.then_branch);
                if let Some(else_id) = s.else_branch {
                    let else_str = self.print_statement(else_id);
                    self.parenthesize("if", &[cond, then, format!("(else {})", else_str)])
                } else {
                    self.parenthesize("if", &[cond, then])
                }
            }
            Stmt::While(s) => {
                let cond = self.print_expression(s.condition);
                let body = self.print_block_statement(s.body);
                self.parenthesize("while", &[cond, body])
            }
        }
    }
    
    fn print_block_statement(&self, block_id: AstId<BlockStmt>) -> String {
        let block = &self.ast.block_stmts[block_id.raw as usize];
        let stmts_str: Vec<String> = block.stmts.iter().map(|&stmt_id| self.print_statement(stmt_id)).collect();
        self.parenthesize("block", &stmts_str)
    }

    // --- Expression Printers ---

    fn print_expression(&self, expr_id: AstId<Expr>) -> String {
        let expr = &self.ast.exprs[expr_id.raw as usize];
        match expr {
            Expr::Assignment(e) => self.parenthesize("=", &[self.print_expression(e.target), self.print_expression(e.value)]),
            Expr::Binary(e) => self.parenthesize(&e.operator.lexeme, &[self.print_expression(e.left), self.print_expression(e.right)]),
            Expr::Unary(e) => self.parenthesize(&e.operator.lexeme, &[self.print_expression(e.right)]),
            Expr::Call(e) => {
                let mut parts = vec![self.print_expression(e.callee)];
                parts.extend(e.args.iter().map(|&arg_id| self.print_expression(arg_id)));
                self.parenthesize("call", &parts)
            },
            Expr::MemberAccess(e) => self.parenthesize(".", &[self.print_expression(e.object), e.field.lexeme.clone()]),
            Expr::StructInit(e) => {
                let fields_str: Vec<String> = e.fields.iter().map(|(name, val_id)| self.parenthesize(&name.lexeme, &[self.print_expression(*val_id)])).collect();
                self.parenthesize(&format!("init {}", &e.name.lexeme), &fields_str)
            },
            Expr::Literal(e) => e.value.lexeme.clone(),
            Expr::Variable(e) => e.name.lexeme.clone(),
            Expr::Grouping(e) => self.parenthesize("group", &[self.print_expression(e.expr)]),
            Expr::UnitLiteral => "()".to_string(),
        }
    }
    
    // --- Type Printer ---
    
    fn print_type(&self, type_id: AstId<Type>) -> String {
        let ty = &self.ast.types[type_id.raw as usize];
        match ty {
            Type::Identifier { name } => name.lexeme.clone(),
            Type::Pointer { base } => self.parenthesize("^", &[self.print_type(*base)]),
            Type::Unit => "()".to_string(),
        }
    }
    
    /// Helper function to generate `(name part1 part2 ...)` style strings.
    fn parenthesize(&self, name: &str, parts: &[String]) -> String {
        let mut result = String::new();
        result.push('(');
        result.push_str(name);
        for part in parts.iter() {
            result.push(' ');
            result.push_str(part);
        }
        result.push(')');
        result
    }
}