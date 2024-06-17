#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::ffi::CString;

mod binaryen;
use crate::binaryen::*;

macro_rules! add_function {
    ($module:expr, { $($token:tt)* }) => {
        let module_str = CString::new(format!("(module {})", stringify!($($token)*))).unwrap();
        let func_mod = BinaryenModuleParse(module_str.as_ptr());
        let func = BinaryenGetFunctionByIndex(func_mod, 0);

        let num_locals = BinaryenFunctionGetNumVars(func);
        let mut local_types = vec![];
        for i in 0..num_locals {
            local_types.push(BinaryenFunctionGetVar(func, i));
        }

        BinaryenAddFunction(
            $module,
            BinaryenFunctionGetName(func),
            BinaryenFunctionGetParams(func),
            BinaryenFunctionGetResults(func),
            local_types.as_ptr() as *mut usize,
            num_locals,
            BinaryenExpressionCopy(BinaryenFunctionGetBody(func), $module)
        );

        BinaryenModuleDispose(func_mod);
    };
}

#[no_mangle]
pub extern "C" fn binaryen_test() {
    unsafe {
        let module = BinaryenModuleCreate();
        println!("module = {}", module as u32);

        add_function!(module, {
            (func $bar (param $0 i32) (result i32)
                (i32.const 72)
            )
        });

        let func = CString::new("foo").expect("fubar");

        BinaryenAddFunction(
            module,
            func.as_ptr(),
            BinaryenTypeInt32(),
            BinaryenTypeInt32(),
            std::ptr::null_mut(),
            0,
            BinaryenConst(module, BinaryenLiteralInt32(43)),
        );
        BinaryenAddFunctionExport(module, func.as_ptr(), func.as_ptr());
        if BinaryenModuleValidate(module) {
            println!("validation ok");
        } else {
            println!("validation error");
            panic!();
        }
        BinaryenModulePrint(module);
        BinaryenModuleDispose(module);
    }
}

fn main() {}
