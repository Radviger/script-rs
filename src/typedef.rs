use std::any::{Any as StdAny, TypeId};
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use engine::{Engine, TypeSignature, FieldSignature, Functions, EvalError};
use std::borrow::Borrow;
use std::fmt::Debug;

pub type TypeNameId = (String, TypeId);
pub type Unit = ();
pub type Array = Vec<ValueRef>;
pub type Object = HashMap<String, ValueRef>;
pub type FnAny = dyn Fn(&mut Vec<ValueRef>, usize, usize) -> Result<ValueRef, EvalError>;

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Unit, Int(i64), Float(f64), Bool(bool), Char(char), String(String)
}

impl ToStringExt for Const {
    fn to_string(&self) -> String {
        match self {
            Const::Unit => ().to_string(),
            Const::Int(v) => ToStringExt::to_string(v),
            Const::Float(v) => ToStringExt::to_string(v),
            Const::Char(v) => ToStringExt::to_string(v),
            Const::Bool(v) => ToStringExt::to_string(v),
            Const::String(v) => ToStringExt::to_string(v)
        }
    }
}

impl Const {
    pub fn value(self) -> Box<dyn Any> {
        match self {
            Const::Unit => Box::new(()),
            Const::Int(v) => Box::new(v),
            Const::Float(v) => Box::new(v),
            Const::Char(v) => Box::new(v),
            Const::Bool(v) => Box::new(v),
            Const::String(v) => Box::new(v)
        }
    }

    pub fn type_id(&self) -> TypeId {
        match self {
            Const::Unit => TypeId::of::<Unit>(),
            Const::Int(_) => TypeId::of::<i64>(),
            Const::Float(_) => TypeId::of::<f64>(),
            Const::Char(_) => TypeId::of::<char>(),
            Const::Bool(_) => TypeId::of::<bool>(),
            Const::String(_) => TypeId::of::<String>()
        }
    }

    pub fn type_name(&self) -> String {
        String::from(match self {
            Const::Unit => <Unit as Type>::type_name(),
            Const::Int(_) => <i64 as Type>::type_name(),
            Const::Float(_) => <f64 as Type>::type_name(),
            Const::Char(_) => <char as Type>::type_name(),
            Const::Bool(_) => <bool as Type>::type_name(),
            Const::String(_) => <String as Type>::type_name()
        })
    }

    pub fn simple_type_name(&self) -> String {
        let full_name = self.type_name();
        if let Some(i) = full_name.rfind("::") {
            full_name.split_at(i).1.to_string()
        } else {
            full_name
        }
    }

    pub fn to_string(&self) -> String {
        match self {
            Const::Unit => "()".to_string(),
            Const::Int(v) => format!("{}", v),
            Const::Float(v) => format!("{}", v),
            Const::Char(v) => format!("{}", v),
            Const::Bool(v) => format!("{}", v),
            Const::String(v) => v.clone()
        }
    }
}

pub trait Parse: Type {
    type ParseResult;

    fn parse(v: Value) -> Option<Self::ParseResult>;
}

pub trait Type {
    fn type_name() -> &'static str;

    fn type_name_of(&self) -> &'static str {
        Self::type_name()
    }
}

impl Type for () {
    fn type_name() -> &'static str {
        "()"
    }
}

impl Type for Box<()> {
    fn type_name() -> &'static str {
        "()"
    }
}

impl Parse for () {
    type ParseResult = ();

    fn parse(v: Value) -> Option<()> {
        match v {
            Value::Const(Const::Unit) => Some(()),
            Value::Generic(mut g) => {
                if let Some(a) = Any::downcast_mut::<Box<()>>(&mut *g) {
                    Some(())
                } else {
                    None
                }
            },
            _ => None
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Const(Const),
    Array(Array),
    Object(Object),
    Generic(Box<dyn Any>)
}

impl ToStringExt for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Const(c) => c.to_string(),
            Value::Array(a) => a.to_string(),
            Value::Object(o) => o.to_string(),
            Value::Generic(g) => g.to_string()
        }
    }
}

pub trait ToStringExt {
    fn to_string(&self) -> String;
}

impl <T: ToStringExt> ToStringExt for Box<T> {
    fn to_string(&self) -> String {
        (*self).to_string()
    }
}

impl <T: ToStringExt> ToStringExt for Vec<T> {
    fn to_string(&self) -> String {
        let mut string = String::from("[");
        let mut first = true;
        for v in self {
            if first {
                string.push_str(&v.to_string());
                first = false;
            } else {
                string.push_str(&format!(", {}", v.to_string()));
            }
        }
        string.push(']');
        string
    }
}

impl <T: ToStringExt> ToStringExt for HashMap<String, T> {
    fn to_string(&self) -> String {
        let mut string = String::from("{");
        let mut first = true;
        for (k, v) in self {
            if first {
                string.push_str(&format!("\"{}\": {}", k, v.to_string()));
                first = false;
            } else {
                string.push_str(&format!(", \"{}\": {}", k, v.to_string()));
            }
        }
        string.push('}');
        string
    }
}

#[derive(Clone)]
pub enum ValueRef {
    Own(Value),
    Ref(Rc<ValueRef>),
    RefMut(Rc<RefCell<ValueRef>>)
}

impl ToStringExt for ValueRef {
    fn to_string(&self) -> String {
        match self {
            ValueRef::Own(v) => v.to_string(),
            ValueRef::Ref(r) => r.to_string(),
            ValueRef::RefMut(r) => {
                let b = (**r).borrow();
                b.to_string()
            }
        }
    }
}

impl ValueRef {
    pub fn generic<T: 'static + Type + Clone + ToStringExt>(value: T) -> ValueRef {
        ValueRef::Own(Value::Generic(Box::new(value)))
    }

    pub fn unit() -> ValueRef {
        Self::own(Value::Const(Const::Unit))
    }

    pub fn own(v: Value) -> ValueRef {
        ValueRef::Own(v)
    }

    pub fn immutable(v: ValueRef) -> ValueRef {
        ValueRef::Ref(Rc::new(v))
    }

    pub fn mutable(v: ValueRef) -> ValueRef {
        ValueRef::RefMut(Rc::new(RefCell::new(v)))
    }

    pub fn type_name(&self) -> String {
        match self {
            ValueRef::Own(v) => v.type_name(),
            ValueRef::Ref(v) => format!("&{}", v.type_name()),
            ValueRef::RefMut(v) => {
                let v = (**v).borrow();
                format!("&mut {}", v.type_name())
            }
        }
    }

    pub fn is_mutable(&self) -> bool {
        match self {
            ValueRef::Own(_) => true,
            ValueRef::RefMut(_) => true,
            _ => false
        }
    }

    pub fn is_ref(&self) -> bool {
        match self {
            ValueRef::Ref(_) => true,
            ValueRef::RefMut(_) => true,
            _ => false
        }
    }

    pub fn deref<F, R, I>(&self, ident: I, line: usize, column: usize, action: F) -> Result<R, EvalError>
        where F: Fn(&Value) -> Result<R, EvalError>, I: Into<String>
    {
        match self {
            ValueRef::Own(value) => action(&value),
            ValueRef::Ref(value) => (*value).deref(ident, line, column, action),
            ValueRef::RefMut(value) => {
                match value.try_borrow() {
                    Ok(value) => (value).deref(ident, line, column, action),
                    Err(_) => Err(EvalError::CannotMutableBorrowVariable(ident.into(), line, column))
                }
            }
        }
    }

    pub fn deref_mut<F, R, I>(&mut self, ident: I, line: usize, column: usize, action: F) -> Result<R, EvalError>
        where F: Fn(&mut Value) -> Result<R, EvalError>, I: Into<String>
    {
        match self {
            ValueRef::Own(ref mut value) => action(value),
            ValueRef::Ref(value) => Err(EvalError::CannotEditImmutableVariable(line, column)),
            ValueRef::RefMut(value) => {
                match value.try_borrow_mut() {
                    Ok(mut value) => (*value).deref_mut(ident, line, column, action),
                    Err(_) => Err(EvalError::CannotMutableBorrowVariable(ident.into(), line, column))
                }
            }
        }
    }
}

impl Value {
    pub fn value(self) -> Box<dyn Any> {
        match self {
            Value::Const(c) => c.value(),
            Value::Array(a) => Box::new(a),
            Value::Object(o) => Box::new(o),
            Value::Generic(g) => g
        }
    }

    pub fn ty(&self) -> (String, TypeId) {
        (self.type_name(), self.type_id())
    }

    pub fn type_id(&self) -> TypeId {
        match self {
            Value::Const(ref c) => c.type_id(),
            Value::Array(_) => TypeId::of::<Array>(),
            Value::Object(_) => TypeId::of::<Object>(),
            Value::Generic(ref e) => {
                Any::type_id(&**e)
            }
        }
    }

    pub fn type_name(&self) -> String {
        match self {
            Value::Const(ref c) => c.type_name(),
            Value::Array(_) => String::from("Array"),
            Value::Object(_) => String::from("Object"),
            Value::Generic(ref e) => String::from(Any::type_name(&**e)),
        }
    }
}

impl Type for Box<dyn Any> {
    fn type_name() -> &'static str {
        "Any"
    }
}

pub trait Any: StdAny + ToStringExt {
    fn type_id(&self) -> TypeId;

    fn type_name(&self) -> &'static str;

    fn box_clone(&self) -> Box<dyn Any>;
}

impl<T> Any for T where T: Clone + StdAny + ?Sized + Type + ToStringExt {
    #[inline]
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }

    #[inline]
    fn type_name(&self) -> &'static str {
        T::type_name()
    }

    #[inline]
    fn box_clone(&self) -> Box<dyn Any> {
        Box::new(self.clone())
    }
}

impl dyn Any {
    #[inline]
    fn box_clone(&self) -> Box<dyn Any> {
        Any::box_clone(self)
    }

    #[inline]
    pub fn is<T: Any>(&self) -> bool {
        let t = TypeId::of::<T>();
        let boxed = Any::type_id(self);

        t == boxed
    }

    #[inline]
    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            unsafe {
                Some(&*(self as *const dyn Any as *const T))
            }
        } else {
            None
        }
    }

    #[inline]
    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            unsafe {
                Some(&mut *(self as *mut dyn Any as *mut T))
            }
        } else {
            None
        }
    }
}

impl Clone for Box<dyn Any> {
    fn clone(&self) -> Self {
        Any::box_clone(self.as_ref() as &dyn Any)
    }
}

#[macro_export]
macro_rules! impl_to_string {
    ($ty:ty) => {
        impl $crate::typedef::ToStringExt for $ty {
            fn to_string(&self) -> String {
                format!("{}", self)
            }
        }
    };
}

impl_to_string!(String);
impl_to_string!(i64);
impl_to_string!(f64);
impl_to_string!(char);
impl_to_string!(bool);

impl ToStringExt for () {
    fn to_string(&self) -> String {
        String::from("()")
    }
}