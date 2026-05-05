use std::collections::HashMap;

use anyhow::{Result, anyhow};

use crate::ast::*;
use crate::database::*;

// ---------------------------------------------------------------------------
// Runtime value
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Str(String),
    Void,
}

impl Value {
    fn as_int(&self) -> Result<i64> {
        match self {
            Value::Int(n) => Ok(*n),
            other => Err(anyhow!("expected int, got {other:?}")),
        }
    }

    fn as_u32(&self) -> u32 {
        match self {
            Value::Int(n) => *n as u32,
            _ => 0,
        }
    }

    fn as_str(&self) -> &str {
        match self {
            Value::Str(s) => s.as_str(),
            _ => "",
        }
    }
}

// ---------------------------------------------------------------------------
// Per-call scope: variables + #define aliases
// ---------------------------------------------------------------------------

struct Scope {
    vars: HashMap<String, Value>,
    /// Maps a name to the canonical variable it aliases (from `#define name target`).
    aliases: HashMap<String, String>,
}

impl Scope {
    fn new() -> Self {
        Scope {
            vars: HashMap::new(),
            aliases: HashMap::new(),
        }
    }

    fn resolve<'a>(&'a self, name: &'a str) -> &'a str {
        self.aliases.get(name).map(|s| s.as_str()).unwrap_or(name)
    }

    fn get(&self, name: &str) -> Value {
        let key = self.resolve(name);
        self.vars.get(key).cloned().unwrap_or(Value::Int(0))
    }

    fn set(&mut self, name: &str, value: Value) {
        let key = self.resolve(name).to_owned();
        self.vars.insert(key, value);
    }

    fn bind_param(&mut self, name: &str, value: Value) {
        self.vars.insert(name.to_owned(), value);
    }

    fn add_alias(&mut self, name: String, target: String) {
        self.aliases.insert(name, target);
    }
}

// ---------------------------------------------------------------------------
// Evaluator
// ---------------------------------------------------------------------------

pub struct Evaluator {
    functions: HashMap<String, FunctionDef>,
    pub db: IdcDatabase,
    next_id: i64,
}

impl Evaluator {
    pub fn new(file: File) -> Self {
        let mut functions = HashMap::new();
        for item in file.items {
            if let Item::FunctionDef(f) = item {
                functions.insert(f.name.clone(), f);
            }
        }
        Evaluator {
            functions,
            db: IdcDatabase::default(),
            next_id: 1000,
        }
    }

    /// Execute the IDC script by calling `main()`.
    pub fn run(&mut self) -> Result<()> {
        if self.functions.contains_key("main") {
            self.call_user_fn("main", vec![])?;
        }
        Ok(())
    }

    fn alloc_id(&mut self) -> i64 {
        let id = self.next_id;
        self.next_id += 1;
        id
    }

    /// Call a user-defined IDC function by name.
    fn call_user_fn(&mut self, name: &str, args: Vec<Value>) -> Result<Value> {
        // Clone to drop the borrow on self.functions before we mutate self.db
        let func = match self.functions.get(name).cloned() {
            Some(f) => f,
            None => return Ok(Value::Void),
        };

        let mut scope = Scope::new();
        for (param, arg) in func.params.iter().zip(args) {
            scope.bind_param(&param.name, arg);
        }

        for stmt in &func.body {
            if let Some(rv) = self.exec_stmt(stmt, &mut scope)? {
                return Ok(rv);
            }
        }
        Ok(Value::Void)
    }

    /// Execute a statement. Returns `Some(value)` only for a `return` statement.
    fn exec_stmt(&mut self, stmt: &Stmt, scope: &mut Scope) -> Result<Option<Value>> {
        match stmt {
            Stmt::Empty => Ok(None),

            Stmt::AutoDecl(names) => {
                for name in names {
                    scope.vars.entry(name.clone()).or_insert(Value::Int(0));
                }
                Ok(None)
            }

            Stmt::Directive(Directive::Define { name, value: Some(target) }) => {
                scope.add_alias(name.clone(), target.clone());
                Ok(None)
            }

            Stmt::Directive(_) => Ok(None),

            Stmt::Expr(expr) => {
                self.eval_expr(expr, scope)?;
                Ok(None)
            }

            Stmt::Return(None) => Ok(Some(Value::Void)),

            Stmt::Return(Some(expr)) => {
                let v = self.eval_expr(expr, scope)?;
                Ok(Some(v))
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr, scope: &mut Scope) -> Result<Value> {
        match expr {
            Expr::Int(n) => Ok(Value::Int(*n)),
            Expr::Str(s) => Ok(Value::Str(s.clone())),
            Expr::Ident(name) => Ok(scope.get(name)),

            Expr::Neg(inner) => {
                let v = self.eval_expr(inner, scope)?.as_int()?;
                Ok(Value::Int(-v))
            }

            Expr::Assign(name, value) => {
                let v = self.eval_expr(value, scope)?;
                scope.set(name, v.clone());
                Ok(v)
            }

            Expr::Call(name, args) => {
                let mut vals = Vec::with_capacity(args.len());
                for arg in args {
                    vals.push(self.eval_expr(arg, scope)?);
                }
                self.call_fn(name, vals)
            }
        }
    }

    /// Dispatch a function call: built-ins first, then user-defined.
    fn call_fn(&mut self, name: &str, args: Vec<Value>) -> Result<Value> {
        match name {
            // ---------------------------------------------------------------
            // No-ops: IDA metadata we don't need
            // ---------------------------------------------------------------
            "DeleteAll"
            | "SetPrcsr"
            | "StringStp"
            | "Tabs"
            | "Comments"
            | "Voids"
            | "XrefShow"
            | "AutoShow"
            | "Indent"
            | "CmtIndent"
            | "TailDepth"
            | "SetEnumBf"
            | "SegClass"
            | "SetSegmentType"
            | "LowVoids"
            | "HighVoids"
            | "ExtLinA"
            | "OpHex"
            | "OpDecimal"
            | "OpBinary"
            | "OpSeg"
            | "SetFunctionFlags"
            | "MakeLocal"
            | "MakeFrame"
            | "SetReg"
            | "AddXref"
            | "AddCodeXref" => Ok(Value::Void),

            // ---------------------------------------------------------------
            // Segments
            // ---------------------------------------------------------------
            "SegCreate" => {
                let start = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let end = args.get(1).map(|v| v.as_u32()).unwrap_or(0);
                let base = args.get(2).map(|v| v.as_u32()).unwrap_or(0);
                self.db.segments.push(IdcSegment {
                    name: String::new(),
                    start,
                    end,
                    base,
                });
                Ok(Value::Void)
            }

            "SegRename" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let name = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                if let Some(seg) = self.db.segments.iter_mut().find(|s| s.start == addr) {
                    seg.name = name;
                }
                Ok(Value::Void)
            }

            // ---------------------------------------------------------------
            // Enums
            // ---------------------------------------------------------------
            "AddEnum" => {
                let name = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                let id = self.alloc_id();
                self.db.enums.insert(
                    id,
                    IdcEnum {
                        name: name.clone(),
                        constants: HashMap::new(),
                    },
                );
                self.db.enum_name_to_id.insert(name, id);
                Ok(Value::Int(id))
            }

            "AddConstEx" => {
                let id = args.first().and_then(|v| v.as_int().ok()).unwrap_or(-1);
                let const_name = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                let value = args.get(2).map(|v| v.as_u32()).unwrap_or(0);
                if let Some(en) = self.db.enums.get_mut(&id) {
                    en.constants.insert(value, const_name);
                }
                Ok(Value::Void)
            }

            "GetEnum" => {
                let name = args.first().map(|v| v.as_str().to_owned()).unwrap_or_default();
                let id = self.db.enum_name_to_id.get(&name).copied().unwrap_or(-1);
                Ok(Value::Int(id))
            }

            // ---------------------------------------------------------------
            // Structs
            // ---------------------------------------------------------------
            "AddStrucEx" => {
                let name = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                let id = self.alloc_id();
                self.db.structs.insert(
                    id,
                    IdcStruct {
                        name: name.clone(),
                        members: Vec::new(),
                    },
                );
                self.db.struct_name_to_id.insert(name, id);
                Ok(Value::Int(id))
            }

            "GetStrucIdByName" => {
                let name = args.first().map(|v| v.as_str().to_owned()).unwrap_or_default();
                let id = self.db.struct_name_to_id.get(&name).copied().unwrap_or(-1);
                Ok(Value::Int(id))
            }

            "AddStrucMember" => {
                let id = args.first().and_then(|v| v.as_int().ok()).unwrap_or(-1);
                let member_name =
                    args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                let offset = args.get(2).map(|v| v.as_u32()).unwrap_or(0);
                let flag = args.get(3).map(|v| v.as_u32()).unwrap_or(0);
                let target_id = args.get(4).and_then(|v| v.as_int().ok()).unwrap_or(-1);
                let size = args.get(5).map(|v| v.as_u32()).unwrap_or(0);
                if let Some(st) = self.db.structs.get_mut(&id) {
                    st.members.push(IdcStructMember {
                        name: member_name,
                        offset,
                        flag,
                        target_id,
                        size,
                    });
                }
                Ok(Value::Void)
            }

            // ---------------------------------------------------------------
            // Byte / code / data attributes
            // ---------------------------------------------------------------
            "MakeCode" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                self.db.attrs.entry(addr).or_default().is_code = true;
                Ok(Value::Void)
            }

            "MakeName" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let name = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                self.db.attrs.entry(addr).or_default().name = Some(name);
                Ok(Value::Void)
            }

            "MakeComm" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let comment = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                self.db.attrs.entry(addr).or_default().comment = Some(comment);
                Ok(Value::Void)
            }

            "MakeRptCmt" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let comment = args.get(1).map(|v| v.as_str().to_owned()).unwrap_or_default();
                self.db.attrs.entry(addr).or_default().repeat_comment = Some(comment);
                Ok(Value::Void)
            }

            "MakeByte" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                self.db.attrs.entry(addr).or_default().data_type = Some("u8".into());
                Ok(Value::Void)
            }

            "MakeWord" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                self.db.attrs.entry(addr).or_default().data_type = Some("u16".into());
                Ok(Value::Void)
            }

            "MakeDword" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                self.db.attrs.entry(addr).or_default().data_type = Some("u32".into());
                Ok(Value::Void)
            }

            "MakeArray" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let count = args.get(1).map(|v| v.as_u32()).unwrap_or(0);
                self.db.attrs.entry(addr).or_default().array_size = Some(count);
                Ok(Value::Void)
            }

            // ---------------------------------------------------------------
            // Operand annotations
            // ---------------------------------------------------------------
            "OpOff" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let op_idx = args.get(1).and_then(|v| v.as_int().ok()).unwrap_or(0);
                let base = args.get(2).map(|v| v.as_u32()).unwrap_or(0);
                // op_idx >= 128 are the "repeatable" (far) variants; store base once per addr
                if op_idx < 128 {
                    self.db.attrs.entry(addr).or_default().ofs_target = Some(base);
                }
                Ok(Value::Void)
            }

            "OpStroffEx" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let struc_id = args.get(2).and_then(|v| v.as_int().ok()).unwrap_or(-1);
                self.db.attrs.entry(addr).or_default().struc_id = Some(struc_id);
                Ok(Value::Void)
            }

            "OpEnumEx" => {
                let addr = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let enum_id = args.get(2).and_then(|v| v.as_int().ok()).unwrap_or(-1);
                self.db.attrs.entry(addr).or_default().enum_id = Some(enum_id);
                Ok(Value::Void)
            }

            // ---------------------------------------------------------------
            // Functions
            // ---------------------------------------------------------------
            "MakeFunction" => {
                let start = args.first().map(|v| v.as_u32()).unwrap_or(0);
                let raw_end = args.get(1).and_then(|v| v.as_int().ok()).unwrap_or(0);
                let end = if raw_end < 0 { 0u32 } else { raw_end as u32 };
                self.db.functions.push(IdcFunction { start, end });
                Ok(Value::Void)
            }

            // ---------------------------------------------------------------
            // Fall through to user-defined functions
            // ---------------------------------------------------------------
            _ => self.call_user_fn(name, args),
        }
    }
}
