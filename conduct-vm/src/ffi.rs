use conduct_tk::AHashMap;

use crate::{rt::Runtime, value::Value};

#[macro_export]
macro_rules! unwrap_args {
    ($args:ident {
        $(
            $(mut $mut_name:ident)? $(let $name:ident)?: $ty:ident;
        )*
    }) => {
        $(
            let $(mut $mut_name)? $($name)? = match $args.get(stringify!($($mut_name)? $($name)?)) {
                Some($crate::value::Value::$ty(v)) => *v,
                _ => return $crate::value::Value::Nil
            };
        )*
    };
}

#[derive(Clone)]
pub struct NativeFunctionDescriptor {
    pub callable: fn(&mut Runtime, AHashMap<String, Value>) -> Value,
    pub arity: Vec<String>,
    pub module: String,
}
