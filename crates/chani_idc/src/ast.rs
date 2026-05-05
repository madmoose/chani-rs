pub type Ident = String;

#[derive(Debug, Clone)]
pub struct File {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Directive(Directive),
    FunctionDef(FunctionDef),
}

#[derive(Debug, Clone)]
pub enum Directive {
    Define { name: Ident, value: Option<String> },
    Include { path: String },
}

#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub name: Ident,
    pub params: Vec<Param>,
    pub body: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Param {
    pub name: Ident,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    AutoDecl(Vec<Ident>),
    Directive(Directive),
    Expr(Expr),
    Return(Option<Expr>),
    Empty,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Ident),
    Int(i64),
    Str(String),
    Neg(Box<Expr>),
    Assign(Ident, Box<Expr>),
    Call(Ident, Vec<Expr>),
}
