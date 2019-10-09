
use std::io::Error as IoError;
use std::fmt::{Debug, Display, Formatter, Error as FmtError};
use std::error::Error;
use std::any::TypeId;
use std::collections::HashMap;
use std::sync::Arc;
use std::rc::Rc;
use std::cell::{Ref, RefCell};
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use std::iter::Iterator;

use parser::{self, ParserError, TokenIterator, FnDef, FnArgDef, Statement, Expression,
             VarType, ExprLoc, StmtLoc, Location, Reference};
use typedef::{Value, ValueRef, Const, Object, Array, Unit, FnAny, Type, Any, Parse, TypeNameId, ToStringExt};

pub type EvalResult = Result<ValueRef, InterruptReason>;

pub enum InterruptReason {
    Return(ValueRef, usize, usize),
    Break(ValueRef, usize, usize),
    Error(EvalError)
}

impl From<EvalError> for InterruptReason {
    fn from(e: EvalError) -> Self {
        InterruptReason::Error(e)
    }
}

#[derive(Debug)]
pub enum EvalError {
    IoError(IoError),
    ParseError(ParserError),
    ReturnTypeMismatch(String, String),
    UnknownFunctionReturnType(String, String, usize, usize),
    UnknownFunctionArgumentType(String, String, usize, usize),
    UnknownType(String, usize, usize),
    UnknownVariable(String, usize, usize),
    UnexpectedBreak(usize, usize),
    MalformedIfGuard(usize, usize),
    MalformedIndexation(String, usize, usize),
    MalformedAssignation(usize, usize),
    InvalidIndex(String, String, usize, usize),
    MissingFunction(String, Vec<(String, TypeId)>, Option<(String, TypeId)>, bool, usize, usize),
    AmbiguousFunctionCall(String, Vec<(String, TypeId)>, bool, usize, usize),
    VariableTypeMismatch(String, String, usize, usize),
    VariableMoved(String, usize, usize, usize, usize),
    CannotBorrowVariable(String, usize, usize),
    CannotMutableBorrowVariable(String, usize, usize),
    CannotEditImmutableVariable(usize, usize),
    CannotMoveBorrowedVariable(String, usize, usize),
    CannotEditBorrowedVariable(String, usize, usize),
    CannotCast(String, usize, usize)
}

impl Display for EvalError {
    fn fmt(&self, f: &mut Formatter) -> Result<(), FmtError> {
        match self {
            EvalError::IoError(e) => write!(f, "IO error: {}", e),
            EvalError::ParseError(e) => write!(f, "Parse error: {}", e),
            EvalError::ReturnTypeMismatch(req, got) => write!(f, "Return type mismatch: required '{}' but got '{}'", req, got),
            EvalError::UnknownFunctionReturnType(fun, ty, line, column) => write!(f, "Unknown return type '{}' for '{}' (line: {}, column: {})", ty, fun, line, column),
            EvalError::UnknownFunctionArgumentType(fun, ty, line, column) => write!(f, "Unknown argument type '{}' for '{}' (line: {}, column: {})", ty, fun, line, column),
            EvalError::UnknownType(ty, line, column) => write!(f, "Unknown type '{}' (line: {}, column: {})", ty, line, column),
            EvalError::UnknownVariable(var, line, column) => write!(f, "Unknown variable: '{}' (line: {}, column: {})", var, line, column),
            EvalError::UnexpectedBreak(line, column) => write!(f, "Unexpected 'break' (line: {}, column: {})", line, column),
            EvalError::MalformedIfGuard(line, column) => write!(f, "If statement guard should be a boolean expression (line: {}, column: {})", line, column),
            EvalError::MalformedIndexation(ident, line, column) => write!(f, "Cannot index non-array variable '{}' (line: {}, column: {})", ident, line, column),
            EvalError::MalformedAssignation(line, column) => write!(f, "Cannot assign to an unsupported left-hand side (line: {}, column: {})", line, column),
            EvalError::InvalidIndex(ident, ty, line, column) => write!(f, "Expected array '{}' index to be 'i64' but got '{}' (line: {}, column: {})", ident, ty, line, column),
            EvalError::MissingFunction(name, args, ret, operator, line, column) => {
                let mut first = true;
                if *operator {
                    write!(f, "Missing operator: '{} {}", args[0].0, name)?;
                    for (n, _) in args.iter().skip(1) {
                        if first {
                            first = false
                        } else {
                            write!(f, ",")?;
                        }
                        write!(f, " {}", n)?;
                    }
                    write!(f, "' (line: {}, column: {})", line, column)
                } else {
                    write!(f, "Missing function: '{}(", name)?;
                    for (n, _) in args {
                        if first {
                            first = false
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", n)?;
                    }
                    write!(f, ")' (line: {}, column: {})", line, column)
                }
            },
            EvalError::AmbiguousFunctionCall(name, args, operator, line, column) => {
                let mut first = true;
                if *operator {
                    write!(f, "Ambiguous operator call: '{} {}", args[0].0, name)?;
                    for (n, _) in args.iter().skip(1) {
                        if first {
                            first = false
                        } else {
                            write!(f, ",")?;
                        }
                        write!(f, " {}", n)?;
                    }
                    write!(f, "' (line: {}, column: {})", line, column)
                } else {
                    write!(f, "Ambiguous function call: '{}(", name)?;
                    for (n, _) in args {
                        if first {
                            first = false
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", n)?;
                    }
                    write!(f, ")' (line: {}, column: {})", line, column)
                }
            },
            EvalError::VariableTypeMismatch(r, fd, line, column) => {
                write!(f, "Variable type mismatch: required '{}' but got '{}' (line: {}, column: {})", r, fd, line, column)
            },
            EvalError::CannotEditImmutableVariable(line, column) => {
                write!(f, "Cannot edit immutable variable (line: {}, column: {})", line, column)
            },
            EvalError::CannotEditBorrowedVariable(name, line, column) => {
                write!(f, "Cannot edit borrowed variable '{}' (line: {}, column: {})", name, line, column)
            },
            EvalError::CannotMoveBorrowedVariable(name, line, column) => {
                write!(f, "Cannot move borrowed variable '{}' (line: {}, column: {})", name, line, column)
            },
            EvalError::CannotCast(ty, line, column) => {
                write!(f, "Cannot cast to '{}' (line: {}, column: {})", ty, line, column)
            },
            EvalError::CannotBorrowVariable(name, line, column) => {
                write!(f, "Cannot borrow variable '{}' because is already mutably borrowed (line: {}, column: {})", name, line, column)
            },
            EvalError::CannotMutableBorrowVariable(name, line, column) => {
                write!(f, "Cannot mutably borrow variable '{}' because is already borrowed (line: {}, column: {})", name, line, column)
            },
            EvalError::VariableMoved(name, to_line, to_column, line, column) => {
                write!(f, "Variable '{}' is moved at {}:{} (line: {}, column: {})", name, to_line, to_column, line, column)
            }
        }
    }
}

impl Error for EvalError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            EvalError::IoError(e) => Some(e),
            EvalError::ParseError(e) => Some(e),
            _ => None
        }
    }
}

impl From<IoError> for EvalError {
    fn from(e: IoError) -> Self {
        EvalError::IoError(e)
    }
}

pub enum Function {
    Internal(Vec<FnArgDef>, Box<StmtLoc>),
    External(Arc<Box<FnAny>>, bool)
}

impl Function {
    pub fn external<F>(fun: F, operator: bool) -> Function
        where F: 'static + Fn(&mut Vec<ValueRef>, usize, usize) -> Result<ValueRef, EvalError>
    {
        Function::External(Arc::new(Box::new(fun)), operator)
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
pub struct FnSignature {
    pub name: String,
    pub args: Vec<Option<(String, TypeId)>>,
    pub ret: Option<(String, TypeId)>
}

impl FnSignature {
    pub fn new<N: Into<String>>(name: N, args: Vec<Option<TypeNameId>>, ret: Option<TypeNameId>) -> FnSignature {
        FnSignature {
            name: name.into(),
            args,
            ret
        }
    }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Hash, Clone, Debug)]
pub struct FieldSignature {
    pub name: String,
    pub ty: (String, TypeId)
}

#[derive(Clone)]
pub struct TypeSignature {
    pub name: String,
    pub ty: TypeId,
    pub functions: Functions,
    pub fields: HashMap<String, FieldSignature>
}

#[derive(Clone)]
pub struct Functions {
    names: HashMap<String, Vec<FnSignature>>,
    descriptors: HashMap<FnSignature, Arc<Function>>
}

impl Functions {
    pub fn new() -> Functions {
        Functions {
            names: HashMap::new(),
            descriptors: HashMap::new()
        }
    }

    pub fn push(&mut self, signature: FnSignature, function: Function) {
        let name = signature.name.clone();
        self.descriptors.insert(signature.clone(), Arc::new(function));
        match self.names.get_mut(&name) {
            Some(specs) => {
                specs.push(signature);
                return;
            },
            _ => {}
        }
        let mut specs = Vec::new();
        specs.push(signature);
        self.names.insert(name, specs);
    }

    pub fn find(&self, name: &String) -> Vec<(FnSignature, Arc<Function>)> {
        let mut result = Vec::new();
        if let Some(matches) = self.names.get(name).cloned() {
            for m in matches {
                if let Some(d) = self.descriptors.get(&m).cloned() {
                    result.push((m, d));
                }
            }
        }
        result
    }

    pub fn find_partial(&self, signature: &FnSignature) -> Vec<(FnSignature, Arc<Function>)> {
        let mut result = Vec::new();
        'd: for (s, d) in &self.descriptors {
            if s.ret != signature.ret && signature.ret.is_some() {
                continue 'd;
            }
            if s.name == signature.name && s.args.len() == signature.args.len() {
                for (i, a) in signature.args.iter().enumerate() {
                    if &s.args[i] != a && a.is_some() {
                        continue 'd;
                    }
                }
                result.push((s.clone(), d.clone()));
            }
        }
        result
    }

    pub fn find_exact(&self, signature: &FnSignature) -> Option<Arc<Function>> {
        self.descriptors.get(signature).cloned()
    }
}

pub struct Types {
    names: HashMap<String, TypeId>,
    descriptors: HashMap<TypeId, TypeSignature>
}

impl Types {
    pub fn new() -> Types {
        Types {
            names: HashMap::new(),
            descriptors: HashMap::new()
        }
    }

    pub fn push(&mut self, signature: TypeSignature) {
        let name = signature.name.clone();
        let ty = signature.ty.clone();
        self.names.insert(name, ty.clone());
        self.descriptors.insert(ty, signature);
    }

    pub fn find(&self, name: &String) -> Option<&TypeSignature> {
        if let Some(id) = self.names.get(name) {
            return self.descriptors.get(id);
        }
        None
    }
}

#[macro_export]
macro_rules! ty {
    ($ty:tt) => {
        (<$ty as Type>::type_name().to_string(), TypeId::of::<$ty>())
    };
}

macro_rules! deref_const_arg {
    ($arg:expr, $name: expr, $const_ty:ident, $fn_name:expr, $line:expr, $column:expr) => {
        $arg.deref($name, $line, $column, |v|{
            if let $crate::Value::Const(Const::$const_ty(a)) = v {
                Ok(*a)
            } else {
                Err($crate::EvalError::UnknownFunctionArgumentType($fn_name.to_string(), v.type_name(), $line, $column))
            }
        })
    }
}

macro_rules! define_cast_operator {
    ($functions:expr, $in_ty:ty, $in_const_ty:ident, $out_ty:ty, $out_const_ty:ident) => {
        $functions.push(
            $crate::FnSignature::new(stringify!(as$out_ty), vec![Some(ty!($in_ty))], Some(ty!($out_ty))),
            $crate::Function::external(|args: &mut Vec<$crate::ValueRef>, line: usize, column: usize| {
                let a = deref_const_arg!(args.pop().unwrap(), "<lhs>", $in_const_ty, stringify!(as$out_ty), line, column)?;
                Ok($crate::ValueRef::Own($crate::Value::Const(Const::$out_const_ty(a as $out_ty))))
            }, true)
        );
    };
}

macro_rules! define_binary_operator {
    ($functions:expr, $name:tt, $ty:ty, $const_ty:ident) => {
        $functions.push(
            $crate::FnSignature::new(stringify!($name), vec![Some(ty!($ty)), Some(ty!($ty))], Some(ty!($ty))),
            $crate::Function::external(|args: &mut Vec<$crate::ValueRef>, line: usize, column: usize| {
                let b = deref_const_arg!(args.pop().unwrap(), "<rhs>", $const_ty, stringify!($name), line, column)?;
                let a = deref_const_arg!(args.pop().unwrap(), "<lhs>", $const_ty, stringify!($name), line, column)?;
                Ok($crate::ValueRef::Own($crate::Value::Const(Const::$const_ty(a $name b))))
            }, true)
        );
    };
}

macro_rules! define_string_concat {
    ($functions:expr, $ty:ty) => {
        $functions.push(
            $crate::FnSignature::new("+", vec![Some(ty!(String)), Some(ty!($ty))], Some(ty!(String))),
            $crate::Function::external(|args: &mut Vec<$crate::ValueRef>, line: usize, column: usize| {
                let b = args.pop().unwrap().deref("<rhs>", line, column, |v| { Ok(v.to_string()) })?;
                let a = args.pop().unwrap().deref("<lhs>", line, column, |v| { Ok(v.to_string()) })?;
                Ok($crate::ValueRef::Own($crate::Value::Const(Const::String(format!("{}{}", a, b)))))
            }, true)
        );
    };
}

macro_rules! define_builtin_type {
    ($engine:expr, $name:ty, $ty:ident) => {
        impl $crate::Type for $name {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }
        impl $crate::Type for Box<$name> {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl $crate::Parse for $name {
            type ParseResult = $name;

            fn parse(value: Value) -> Option<$name> {
                use $crate::Any;

                match value {
                    $crate::Value::$ty(val) => Some(val),
                    $crate::Value::Generic(mut g) => {
                        if let Some(a) = Any::downcast_mut::<Box<$name>>(&mut *g) {
                            Some(*a.clone())
                        } else {
                            None
                        }
                    },
                    _ => None
                }
            }
        }

        $engine.register_type::<$name>(stringify!($name), None, None);
    };
}

macro_rules! define_builtin_const_type {
    ($engine:expr, $name:ty, $ty:ident) => {
        impl $crate::Type for $name {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }
        impl $crate::Type for &$name {
            fn type_name() -> &'static str {
                stringify!(&$name)
            }
        }
        impl $crate::Type for &mut $name {
            fn type_name() -> &'static str {
                stringify!(&mut $name)
            }
        }
        impl $crate::Type for Box<$name> {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl $crate::Parse for $name {
            type ParseResult = $name;

            fn parse(value: Value) -> Option<$name> {
                use $crate::Any;

                match value {
                    $crate::Value::Const($crate::Const::$ty(val)) => Some(val),
                    $crate::Value::Generic(mut g) => {
                        if let Some(a) = Any::downcast_mut::<Box<$name>>(&mut *g) {
                            Some(*a.clone())
                        } else {
                            None
                        }
                    },
                    _ => None
                }
            }
        }

        $engine.register_type::<$name>(stringify!($name), None, None);
    };
}

#[macro_export]
macro_rules! define_type {
    ($engine:expr, $name:ty) => {
        define_type!($engine, $name, None, None);
    };
    ($engine:expr, $name:ty, $functions:expr) => {
        define_type!($engine, $name, $functions, None);
    };
    ($engine:expr, $name:ty, $functions:expr, $fields:expr) => {
        impl $crate::Type for $name {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }
        impl $crate::Type for Box<$name> {
            fn type_name() -> &'static str {
                stringify!($name)
            }
        }

        impl $crate::Parse for $name {
            type ParseResult = $name;

            fn parse(value: $crate::Value) -> Option<$name> {
                use $crate::Any;

                match value {
                    $crate::Value::Generic(mut g) => {
                        if let Some(a) = Any::downcast_mut::<$name>(&mut *g) {
                            Some(a.clone())
                        } else {
                            None
                        }
                    },
                    _ => None
                }
            }
        }

        $engine.register_type::<$name>(stringify!($name), $functions, $fields);
    };
}



pub struct Engine {
    pub functions: Functions,
    pub types: Types,
    pub constants: HashMap<String, Value>
}

impl Engine {
    pub fn new() -> Engine {
        let mut functions = Functions::new();

        define_binary_operator!(functions, +, i64, Int);
        define_binary_operator!(functions, +, f64, Float);
        define_binary_operator!(functions, -, i64, Int);
        define_binary_operator!(functions, -, f64, Float);
        define_binary_operator!(functions, *, i64, Int);
        define_binary_operator!(functions, *, f64, Float);
        define_binary_operator!(functions, /, i64, Int);
        define_binary_operator!(functions, /, f64, Float);
        define_binary_operator!(functions, %, i64, Int);
        define_binary_operator!(functions, %, f64, Float);
        define_binary_operator!(functions, >>, i64, Int);
        define_binary_operator!(functions, <<, i64, Int);
        define_binary_operator!(functions, |, i64, Int);
        define_binary_operator!(functions, &, i64, Int);
        define_binary_operator!(functions, ^, i64, Int);

        define_cast_operator!(functions, i64, Int, i64, Int);
        define_cast_operator!(functions, i64, Int, f64, Float);
        define_cast_operator!(functions, f64, Float, f64, Float);
        define_cast_operator!(functions, f64, Float, i64, Int);

        define_string_concat!(functions, String);
        define_string_concat!(functions, char);
        define_string_concat!(functions, i64);
        define_string_concat!(functions, f64);

        let mut types = Types::new();
        let mut constants = HashMap::new();

        let mut engine = Engine { functions, types, constants };

        define_builtin_type!(engine, Array, Array);
        define_builtin_type!(engine, Object, Object);
        define_builtin_const_type!(engine, String, String);
        define_builtin_const_type!(engine, i64, Int);
        define_builtin_const_type!(engine, f64, Float);
        define_builtin_const_type!(engine, bool, Bool);
        define_builtin_const_type!(engine, char, Char);

        engine.register_type::<()>("()", None, None);

        engine
    }

    pub fn register_type<T>(&mut self, name: &str, functions: Option<Functions>, fields: Option<HashMap<String, FieldSignature>>)
        where T: Any + Clone + Type
    {
        let functions = functions.unwrap_or(Functions::new());
        let fields = fields.unwrap_or(HashMap::new());

        self.types.push(TypeSignature {
            name: name.to_string(),
            ty: TypeId::of::<T>(),
            functions: functions.clone(),
            fields: fields.clone()
        });
        self.types.push(TypeSignature {
            name: format!("&{}", name.to_string()),
            ty: TypeId::of::<&T>(),
            functions: functions.clone(),
            fields: fields.clone()
        });
        self.types.push(TypeSignature {
            name: format!("&mut {}", name.to_string()),
            ty: TypeId::of::<&mut T>(),
            functions,
            fields
        });
    }

    pub fn eval_file<T, P>(&mut self, path: P) -> Result<T::ParseResult, EvalError>
        where T: Any + Clone + Parse, P: AsRef<Path>
    {
        let mut file = File::open(path)?;
        let mut source = String::new();
        file.read_to_string(&mut source)?;

        self.eval::<T>(&source)
    }

    pub fn eval<T>(&mut self, input: &str) -> Result<T::ParseResult, EvalError>
        where T: Any + Clone + Parse
    {
        let result = self.eval_raw(input)?;
        let result_type_name = result.type_name();
        if let Some(result) = T::parse(result) {
            Ok(result)
        } else {
            Err(EvalError::ReturnTypeMismatch(<T as Type>::type_name().to_string(), result_type_name))
        }
    }

    pub fn eval_raw(&mut self, input: &str) -> Result<Value, EvalError> {
        let mut line = Rc::new(RefCell::new(1usize));
        let mut column = Rc::new(RefCell::new(1usize));
        let mut input = TokenIterator::from(input, &mut line, &mut column).peekable();
        let mut scope = Scope::new();

        match parser::parse(&mut input, Location(line, column)) {
            Ok((functions, statements)) => {
                for f in functions {
                    let FnDef { location, name, args, ret, body } = f;
                    let ret = if let Some((r, line, column)) = ret {
                        if let Some(s) = self.types.find(&r) {
                            Some((s.name.clone(), s.ty.clone()))
                        } else {
                            return Err(EvalError::UnknownFunctionReturnType(name, r, line, column))
                        }
                    } else {
                        None
                    };
                    let args_info = args.clone();
                    let args = {
                        let mut l = Vec::new();
                        for a in args {
                            l.push(if let Some(ty) = a.ty {
                                if let Some(s) = self.types.find(&ty) {
                                    Some((s.name.clone(), s.ty.clone()))
                                } else {
                                    let (line, column) = a.location;
                                    return Err(EvalError::UnknownFunctionArgumentType(name, ty, line, column))
                                }
                            } else {
                                None
                            });
                        }
                        l
                    };
                    let signature = FnSignature { name, args, ret };
                    self.functions.push(signature, Function::Internal(args_info, body))
                }
                let mut tmp = ValueRef::unit();
                let mut loc = (0usize, 0usize);
                for s in statements {
                    let (line, column) = s.location;
                    match self.eval_statement(&mut scope, s) {
                        Ok(val) => {
                            tmp = val;
                            loc = (line, column);
                        },
                        Err(e) => match e {
                            InterruptReason::Return(r, line, column) => {
                                tmp = r;
                                loc = (line, column);
                            },
                            InterruptReason::Break(r, line, column) => {
                                tmp = r;
                                loc = (line, column);
                            }
                            InterruptReason::Error(e) => return Err(e)
                        }
                    }
                }
                tmp.deref("<return value>", loc.0, loc.1, |v|Ok(v.clone()))
            },
            Err(e) => Err(EvalError::ParseError(e)),
        }
    }

    fn eval_statement(&mut self, scope: &mut Scope, statement: StmtLoc) -> EvalResult {
        let StmtLoc { location, statement } = statement;
        let (line, column) = location;
        match statement {
            Statement::Expression(expr) => self.eval_expression(scope, *expr),
            Statement::If(guard, body) => self.eval_if(scope, *guard, *body, None),
            Statement::IfElse(guard, main_branch, else_branch) => self.eval_if(scope, *guard, *main_branch, Some(*else_branch)),
            Statement::Let(ident, ty, value) => self.eval_let(scope, ident, ty, *value),
            Statement::Block(statements) => self.eval_block(scope, statements),
            Statement::Break(result) => {
                let result = if let Some(result) = result {
                    self.eval_expression(scope, *result)?
                } else {
                    ValueRef::unit()
                };
                Err(InterruptReason::Break(result, line, column))
            },
            Statement::Return(result) => {
                let result = if let Some(result) = result {
                    self.eval_expression(scope, *result)?
                } else {
                    ValueRef::unit()
                };
                Err(InterruptReason::Return(result, line, column))
            }
        }
    }

    fn eval_block(&mut self, scope: &mut Scope, statements: Vec<StmtLoc>) -> EvalResult {
        scope.new_level();
        let mut x = ValueRef::unit();
        for s in statements {
            match self.eval_statement(scope, s) {
                Ok(val) => x = val,
                Err(e) => {
                    scope.pop_level();
                    return Err(e);
                }
            }
        }
        scope.pop_level();
        Ok(x)
    }

    fn eval_let(&mut self, scope: &mut Scope, ident: String, ty: VarType, value: ExprLoc) -> EvalResult {
        let (line, column) = value.location;
        let value = self.eval_expression(scope, value)?;
        if let VarType::Defined(req_ty, r) = ty {
            if self.types.find(&req_ty).is_none() {
                return Err(EvalError::UnknownType(req_ty, line, column).into());
            }
            let ty = value.type_name();
            if req_ty != ty {
                return Err(EvalError::VariableTypeMismatch(req_ty, ty, line, column).into());
            }
        }
        scope.push(ident, value);
        Ok(ValueRef::unit())
    }

    fn eval_if(&mut self, scope: &mut Scope, guard: ExprLoc, main_branch: StmtLoc,
               else_branch: Option<StmtLoc>) -> EvalResult {

        let (line, column) = guard.location;

        let guard = self.eval_expression(scope, guard)?;
        let guard = guard.deref("<if guard>", line, column, |val| {
            match val {
                Value::Const(Const::Bool(guard)) => Ok(*guard),
                _ => Err(EvalError::MalformedIfGuard(line, column).into())
            }
        })?;
        if guard {
            self.eval_statement(scope, main_branch)
        } else if let Some(else_branch) = else_branch {
            self.eval_statement(scope, else_branch)
        } else {
            Ok(ValueRef::unit())
        }
    }

    fn eval_cast(&mut self, scope: &mut Scope, what: ExprLoc, to: String) -> EvalResult {
        let (line, column) = what.location;
        if let Some(s) = self.types.find(&to).cloned() {
            let target_ty = s.name;
            let fn_name = format!("as {}", target_ty);
            match self.eval_expression(scope, ExprLoc {
                expression: Expression::Invoke(None, fn_name.clone(), vec![what], Some(target_ty.clone()), true),
                location: (line, column)
            }) {
                Ok(val) => Ok(val),
                Err(e) => {
                    match e {
                        InterruptReason::Error(e) => {
                            match e {
                                EvalError::MissingFunction(name, args, ret, operator, i, c) => {
                                    if name == fn_name {
                                        Err(EvalError::CannotCast(target_ty, line, column).into())
                                    } else {
                                        Err(EvalError::MissingFunction(name, args, ret, operator, i, c).into())
                                    }
                                },
                                other => Err(other.into())
                            }
                        },
                        other => Err(other)
                    }
                }
            }
        } else {
            Err(EvalError::UnknownType(to, line, column).into())
        }
    }

    fn eval_expression(&mut self, scope: &mut Scope, expression: ExprLoc) -> EvalResult {
        let ExprLoc { location, expression } = expression;
        let (line, column) = location;
        match expression {
            Expression::Const(c) => Ok(ValueRef::own(Value::Const(c))),
            Expression::Array(elements) => self.eval_array(scope, elements),
            Expression::Object(properties) => self.eval_object(scope, properties),
            Expression::Identifier(i) => Ok(scope.mov(&i, line, column)?),
            Expression::Reference(i, r) => {
                match r {
                    Reference::Immutable => {
                        let val = scope.borrow(&i, line, column)?;
                        Ok(ValueRef::Ref(val))
                    },
                    Reference::Mutable => {
                        let val = scope.borrow_mut(&i, line, column)?;
                        Ok(ValueRef::RefMut(val))
                    }
                }
            }
            Expression::Assign(what, to) => self.eval_assign(scope, *what, *to),
            Expression::Invoke(ty, ident, args, ret, operator) => {
                if let Some(ty) = ty {
                    if let Some(s) = self.types.find(&ty).cloned() {
                        self.eval_invocation(scope, Some(s.functions), ident, args, ret, operator, line, column)
                    } else {
                        Err(EvalError::UnknownType(ty, line, column).into())
                    }
                } else {
                    self.eval_invocation(scope, None, ident, args, ret, operator, line, column)
                }
            },
            /*Expression::Index(ident, index) => self.eval_indexation(scope, ident, index),*/
            Expression::UnaryOperator(op, arg) => {
                self.eval_expression(scope, ExprLoc {
                    expression: Expression::Invoke(None, op.sign(), vec![*arg], None, true),
                    location: (line, column)
                })
            },
            Expression::BinaryOperator(op, a, b) => {
                self.eval_expression(scope, ExprLoc {
                    expression: Expression::Invoke(None, op.sign(), vec![*a, *b], None, true),
                    location: (line, column)
                })
            },
            Expression::Cast(what, to) => self.eval_cast(scope, *what, to),
            _ => unimplemented!()
        }
    }

    fn eval_array(&mut self, scope: &mut Scope, elements: Vec<ExprLoc>) -> EvalResult {
        let mut result = Vec::new();
        for e in elements {
            result.push(self.eval_expression(scope, e)?);
        }
        Ok(ValueRef::Own(Value::Array(result)))
    }

    fn eval_object(&mut self, scope: &mut Scope, properties: HashMap<String, ExprLoc>) -> EvalResult {
        let mut result = HashMap::new();
        for (k, p) in properties {
            result.insert(k, self.eval_expression(scope, p)?);
        }
        Ok(ValueRef::Own(Value::Object(result)))
    }

    fn eval_assign(&mut self, scope: &mut Scope, what: ExprLoc, to: ExprLoc) -> EvalResult {
        let to = self.eval_expression(scope, to)?;
        match what {
            ExprLoc { expression: Expression::Identifier(ident), location: (line, column) } => {
                scope.assign(ident, to, line, column)?;
                Ok(ValueRef::unit())
            },
            ExprLoc { expression: Expression::Index(ident, index), location: (line, column) } => {
                let index = self.eval_expression(scope, *index)?;
                let index = index.deref("<array index>", line, column, |val| {
                    match val {
                        Value::Const(Const::Int(v)) => Ok(*v as usize),
                        o => Err(EvalError::InvalidIndex(ident.clone(), o.type_name(), line, column).into())
                    }
                })?;
                Ok(scope.find_mut(&ident, line, column, |array| {
                    Ok(array.deref_mut(&ident, line, column, |val| {
                        match val {
                            Value::Array(elements) => {
                                elements.insert(index, to.clone());
                                Ok(ValueRef::unit())
                            },
                            _ => Err(EvalError::MalformedIndexation(ident.clone(), line, column).into())
                        }
                    })?)
                })?)
            },
            ExprLoc { expression: Expression::Dot(left, right), location } => {
                unimplemented!()
            },
            ExprLoc { location: (line, column), .. } => Err(EvalError::MalformedAssignation(line, column).into()),
        }
    }

    fn invoke(&mut self, scope: &mut Scope, args: &mut Vec<ValueRef>, ident: String, fun: Arc<Function>,
              arg_types: Vec<(String, TypeId)>, ret: Option<(String, TypeId)>, line: usize, column: usize) -> EvalResult {

        match &*fun {
            &Function::Internal(ref args_info, ref body) => {
                println!("Invoking internal function {} ({} args; {} args info)", ident, args.len(), args_info.len());
                scope.new_level();
                args.reverse();
                for (i, a) in args_info.iter().enumerate() {
                    scope.push(a.name.clone(), args.pop().unwrap());
                }
                let result = match self.eval_statement(scope, *body.clone()) {
                    Ok(result) => Ok(result),
                    Err(e) => match e {
                        InterruptReason::Return(result, _, _) => Ok(result),
                        InterruptReason::Break(_, line, column) => Err(EvalError::UnexpectedBreak(line, column).into()),
                        other => Err(other)
                    }
                };

                scope.pop_level();
                result
            },
            &Function::External(ref r, ..) => Ok((*r)(args, line, column)?),
        }
    }

    fn eval_invocation(&mut self, scope: &mut Scope, repo: Option<Functions>, ident: String, args: Vec<ExprLoc>,
                       ret: Option<String>, operator: bool, line: usize, column: usize) -> EvalResult {

        let ret = if let Some(ret) = ret {
            if let Some(s) = self.types.find(&ret) {
                Some((s.name.clone(), s.ty.clone()))
            } else {
                return Err(EvalError::UnknownFunctionReturnType(ident, ret, line, column).into());
            }
        } else {
            None
        };

        let (mut args, arg_types) = {
            let mut result = Vec::new();
            let mut types = Vec::new();
            for a in args {
                let (line, column) = a.location;
                let a = self.eval_expression(scope, a)?;
                let ty = a.type_name();
                if let Some(s) = self.types.find(&ty) {
                    types.push((s.name.clone(), s.ty.clone()));
                    result.push(a);
                } else {
                    return Err(EvalError::UnknownType(ty, line, column).into());
                }
            }
            (result, types)
        };
        let mut functions = Vec::new();
        {
            let repo = repo.as_ref().unwrap_or(&self.functions);
            'd: for (s, f) in repo.find(&ident) {
                if s.args.len() == args.len() {
                    if ret != s.ret && ret.is_some() {
                        continue 'd;
                    }
                    for (i, a) in s.args.iter().enumerate() {
                        if a.as_ref() != arg_types.get(i) && a.is_some() {
                            continue 'd;
                        }
                    }
                    functions.push((s, f));
                }
            }
        }
        let len = functions.len();
        match len {
            0 => Err(EvalError::MissingFunction(ident, arg_types, ret, operator, line, column).into()),
            1 => {
                let (s, fun) = functions.pop().unwrap();
                self.invoke(scope, &mut args, ident, fun, arg_types, ret, line, column)
            },
            _ => Err(EvalError::AmbiguousFunctionCall(ident, arg_types, operator, line, column).into())
        }
    }
}

pub enum ScopeVariable {
    Free(ValueRef),
    Borrowed(Rc<ValueRef>),
    BorrowedMut(Rc<RefCell<ValueRef>>),
    Moved(usize, usize)
}

pub struct Scope {
    inner: Vec<HashMap<String, ScopeVariable>>
}

impl Scope {
    pub fn new() -> Scope {
        Scope { inner: Vec::new() }
    }

    pub fn depth(&self) -> usize {
        self.inner.len()
    }

    pub fn new_level(&mut self) {
        self.inner.push(HashMap::new())
    }

    pub fn pop_level(&mut self) -> bool {
        self.inner.pop().is_some()
    }

    pub fn push(&mut self, name: String, value: ValueRef) {
        let value = ScopeVariable::Free(value);
        match self.inner.last_mut() {
            Some(last) => {
                last.insert(name, value);
                return;
            },
            _ => {}
        }
        let mut new_level = HashMap::new();
        new_level.insert(name, value);
        self.inner.push(new_level);
    }

    pub fn find_mut<F, R>(&mut self, name: &String, line: usize, column: usize, action: F) -> Result<R, EvalError>
        where F: FnOnce(&mut ValueRef) -> Result<R, EvalError>
    {
        for l in self.inner.iter_mut().rev() {
            if let Some(v) = l.remove(name) {
                return match v {
                    ScopeVariable::Free(mut v) => {
                        let result = action(&mut v);
                        l.insert(name.clone(), ScopeVariable::Free(v));
                        result
                    },
                    ScopeVariable::Borrowed(iv) => {
                        if let Ok(mut iv) = Rc::try_unwrap(iv) {
                            let result = action(&mut iv);
                            l.insert(name.clone(), ScopeVariable::Free(iv));
                            result
                        } else {
                            Err(EvalError::CannotEditBorrowedVariable(name.clone(), line, column))
                        }
                    },
                    ScopeVariable::BorrowedMut(iv) => {
                        if let Ok(mut iv) = Rc::try_unwrap(iv) {
                            let mut iv = iv.into_inner();
                            let result = action(&mut iv);
                            l.insert(name.clone(), ScopeVariable::Free(iv));
                            result
                        } else {
                            Err(EvalError::CannotEditBorrowedVariable(name.clone(), line, column))
                        }
                    },
                    ScopeVariable::Moved(to_line, to_column) => {
                        l.insert(name.clone(), ScopeVariable::Moved(line, column));
                        Err(EvalError::VariableMoved(name.clone(), to_line, to_column, line, column))
                    }
                }
            }
        }
        Err(EvalError::UnknownVariable(name.clone(), line, column))
    }

    pub fn assign(&mut self, name: String, to: ValueRef, line: usize, column: usize) -> Result<(), EvalError> {
        self.find_mut(&name, line, column, move |v|{ *v = to; Ok(()) })
    }

    pub fn mov(&mut self, name: &String, line: usize, column: usize) -> Result<ValueRef, EvalError> {
        for l in self.inner.iter_mut().rev() {
            if let Some(v) = l.remove(name) {
                return match v {
                    ScopeVariable::Free(inner) => {
                        l.insert(name.clone(), ScopeVariable::Moved(line, column));
                        Ok(inner)
                    },
                    ScopeVariable::Borrowed(iv) => {
                        if let Ok(mut iv) = Rc::try_unwrap(iv) {
                            l.insert(name.clone(), ScopeVariable::Moved(line, column));
                            Ok(iv)
                        } else {
                            Err(EvalError::CannotMoveBorrowedVariable(name.clone(), line, column))
                        }
                    },
                    ScopeVariable::BorrowedMut(iv) => {
                        if let Ok(mut iv) = Rc::try_unwrap(iv) {
                            let mut iv = iv.into_inner();
                            l.insert(name.clone(), ScopeVariable::Moved(line, column));
                            Ok(iv)
                        } else {
                            Err(EvalError::CannotMoveBorrowedVariable(name.clone(), line, column))
                        }
                    },
                    ScopeVariable::Moved(to_line, to_column) => {
                        l.insert(name.clone(), ScopeVariable::Moved(line, column));
                        Err(EvalError::VariableMoved(name.clone(), to_line, to_column, line, column))
                    }
                }
            }
        }
        Err(EvalError::UnknownVariable(name.clone(), line, column))
    }

    pub fn borrow(&mut self, name: &String, line: usize, column: usize) -> Result<Rc<ValueRef>, EvalError> {
        for l in self.inner.iter_mut().rev() {
            if let Some(v) = l.remove(name) {
                match v {
                    ScopeVariable::Free(val) => {
                        let r = Rc::new(val);
                        l.insert(name.clone(), ScopeVariable::Borrowed(r.clone()));
                        return Ok(r);
                    },
                    ScopeVariable::Borrowed(val) => {
                        l.insert(name.clone(), ScopeVariable::Borrowed(val.clone()));
                        return Ok(val);
                    },
                    ScopeVariable::BorrowedMut(val) => {
                        l.insert(name.clone(), ScopeVariable::BorrowedMut(val));
                        return Err(EvalError::CannotBorrowVariable(name.clone(), line, column));
                    },
                    ScopeVariable::Moved(to_line, to_column) => {
                        return Err(EvalError::VariableMoved(name.clone(), to_line, to_column, line, column))
                    }
                }
            }
        }
        Err(EvalError::UnknownVariable(name.clone(), line, column))
    }

    pub fn borrow_mut(&mut self, name: &String, line: usize, column: usize) -> Result<Rc<RefCell<ValueRef>>, EvalError> {
        for l in self.inner.iter_mut().rev() {
            if let Some(v) = l.remove(name) {
                match v {
                    ScopeVariable::Free(val) => {
                        let r = Rc::new(RefCell::new(val));
                        l.insert(name.clone(), ScopeVariable::BorrowedMut(r.clone()));
                        return Ok(r);
                    },
                    ScopeVariable::Borrowed(val) => {
                        l.insert(name.clone(), ScopeVariable::Borrowed(val.clone()));
                        return Err(EvalError::CannotMutableBorrowVariable(name.clone(), line, column));
                    },
                    ScopeVariable::BorrowedMut(val) => {
                        l.insert(name.clone(), ScopeVariable::BorrowedMut(val));
                        return Err(EvalError::CannotBorrowVariable(name.clone(), line, column));
                    },
                    ScopeVariable::Moved(to_line, to_column) => {
                        return Err(EvalError::VariableMoved(name.clone(), to_line, to_column, line, column))
                    }
                }
            }
        }
        Err(EvalError::UnknownVariable(name.clone(), line, column))
    }
}