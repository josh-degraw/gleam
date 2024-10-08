#! /bin/bash
cargo build && \
cd external/stdlib && \
rm -rf ./build/dev/fsharp && \
../../target/debug/gleam test --target fsharp
