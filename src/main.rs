#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

mod frontend;

#[no_mangle]
pub extern "C" fn compile(input: *const u8, input_len: usize) {
    let input = unsafe { std::slice::from_raw_parts(input, input_len) };
    let input = std::str::from_utf8(input).unwrap();
    println!("Compiling input: {}", input);

    let tokens = frontend::tokenizer::tokenize(input);
    println!("{:?}", tokens);

    let ast = frontend::parser::parse(tokens);
    println!("{:?}", ast);
}

fn main() {
    let input = "
    fn main(): i32 {
        let x: i32 = 42;
        let y: i32 = 43;
        let z = x + y;
        return z;
    }
    ";

    compile(input.as_ptr(), input.len());
}
