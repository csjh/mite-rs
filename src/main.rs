mod binaryen;
mod frontend;

use crate::binaryen::*;

#[no_mangle]
pub extern "C" fn compile(input: *const u8, input_len: usize, cb: extern "C" fn(*const u8, usize)) {
    let input = unsafe { std::slice::from_raw_parts(input, input_len) };
    let input = std::str::from_utf8(input).unwrap();
    println!("Compiling input: {}", input);

    let tokens = frontend::tokenizer::tokenize(input);
    println!("{:?}", tokens);

    let ast = frontend::parser::parse(tokens);
    println!("{:?}", ast);

    let ir = frontend::ir::ast_to_ir(ast);

    cb(input.as_ptr(), input.len());
}

fn main() {
    let _input = "
    fn main(): i32 {
        let x: i32 = 42;
        let y: i32 = 43;
        let z = x + y;
        return z;
    }
    ";

    // compile(input.as_ptr(), input.len());

    let module = unsafe { BinaryenModuleCreate() };
    unsafe { BinaryenModuleDispose(module) };
}
