extern crate bindgen;

fn main() {
    // add link-arg if in dev mode
    let profile = std::env::var("PROFILE").unwrap();
    match profile.as_str() {
        "debug" => {
            println!("cargo:rustc-link-arg=-sERROR_ON_WASM_CHANGES_AFTER_LINK");
        }
        _ => {}
    }

    #[cfg(feature = "bindings")]
    {
        bindgen::Builder::default()
            // https://github.com/rust-lang/rust-bindgen/issues/1941
            .clang_arg("-fvisibility=default")
            .header("./binaryen/src/binaryen-c.h")
            .merge_extern_blocks(true)
            .raw_line("#![allow(unused)]")
            .raw_line("#![allow(non_upper_case_globals)]")
            .raw_line("#![allow(non_camel_case_types)]")
            .raw_line("#![allow(non_snake_case)]")
            .generate()
            .expect("Unable to generate bindings")
            .write_to_file("./src/binaryen.rs")
            .expect("Couldn't write bindings!");

        panic!("Generated bindings succesfully. Rerun without `--features 'bindings'` to build the project.")
    }
}
