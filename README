dev env setup:
- run `cd binaryen && emcmake cmake -DBUILD_STATIC_LIB=ON -DBUILD_TOOLS=OFF -DBUILD_TESTS=OFF -DENABLE_BIGINT=ON -DCMAKE_BUILD_TYPE=Debug . && emmake make binaryen`
- run `cargo build`

update binaryen bindings:
- run `cargo build --features "bindings" --target wasm32-unknown-unknown` - this should panic
