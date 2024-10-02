#! /bin/bash
cargo build && cd external/stdlib
../../target/debug/gleam test --target fsharp
