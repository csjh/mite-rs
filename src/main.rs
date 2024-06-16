use std::ffi::{c_char, c_void, CString};

#[repr(C)]
pub struct BinaryenLiteral {
    pub type_: usize,
    pub value: BinaryenLiteralValue,
}

#[repr(C)]
pub union BinaryenLiteralValue {
    pub i32: i32,
    pub i64: i64,
    pub f32: f32,
    pub f64: f64,
    pub v128: [u8; 16],
    pub func: *const c_char,
}

type BinaryenModuleRef = *const c_void;
type BinaryenType = *const c_void;
type BinaryenExpressionRef = *const c_void;
type BinaryenFunctionRef = *const c_void;
type BinaryenIndex = u32;

extern "C" {
    fn BinaryenModuleCreate() -> BinaryenModuleRef;
    fn BinaryenModuleDispose(module: BinaryenModuleRef);
    fn BinaryenAddFunction(
        module: BinaryenModuleRef,
        name: *const c_char,
        params: BinaryenType,
        results: BinaryenType,
        varTypes: *const BinaryenType,
        numVarTypes: BinaryenIndex,
        body: BinaryenExpressionRef,
    ) -> BinaryenFunctionRef;
    fn BinaryenAddFunctionExport(
        module: BinaryenModuleRef,
        internalName: *const c_char,
        externalName: *const c_char,
    );
    fn BinaryenModuleValidate(module: BinaryenModuleRef) -> bool;
    fn BinaryenModulePrint(module: BinaryenModuleRef);
    fn BinaryenTypeInt32() -> BinaryenType;
    fn BinaryenConst(module: BinaryenModuleRef, value: BinaryenLiteral) -> BinaryenExpressionRef;
    fn BinaryenLiteralInt32(x: i32) -> BinaryenLiteral;

    fn BinaryenModuleParse(text: *const c_char) -> BinaryenModuleRef;
    fn BinaryenFunctionGetNumVars(func: BinaryenFunctionRef) -> BinaryenIndex;
    fn BinaryenFunctionGetVar(func: BinaryenFunctionRef, index: BinaryenIndex) -> BinaryenType;
    fn BinaryenFunctionGetName(func: BinaryenFunctionRef) -> *const c_char;
    fn BinaryenFunctionGetParams(func: BinaryenFunctionRef) -> BinaryenType;
    fn BinaryenFunctionGetResults(func: BinaryenFunctionRef) -> BinaryenType;
    fn BinaryenFunctionGetBody(func: BinaryenFunctionRef) -> BinaryenExpressionRef;
    fn BinaryenGetFunctionByIndex(
        module: BinaryenModuleRef,
        index: BinaryenIndex,
    ) -> BinaryenFunctionRef;
    fn BinaryenExpressionCopy(
        expr: BinaryenExpressionRef,
        module: BinaryenModuleRef,
    ) -> BinaryenExpressionRef;
}

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
            local_types.as_ptr(),
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
