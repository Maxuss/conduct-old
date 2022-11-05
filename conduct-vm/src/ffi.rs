use conduct_tk::AHashMap;

use crate::{reg::StackValue, vm::Vm};

#[macro_export]
macro_rules! unwrap_args {
    ($args:ident {
        $(
            $(mut $mut_name:ident)? $(let $name:ident)?: $ty:ident;
        )*
    }) => {
        $(
            let $(mut $mut_name)? $($name)? = match $args.get(stringify!($($mut_name)? $($name)?)) {
                Some($crate::reg::StackValue::$ty(v)) => *v,
                _ => return $crate::reg::StackValue::Nil
            };
        )*
    };
}

#[derive(Clone)]
pub struct NativeFunctionDescriptor {
    pub callable: fn(&mut Vm, AHashMap<String, StackValue>) -> StackValue,
    pub arity: Vec<String>,
    pub module: String,
}
