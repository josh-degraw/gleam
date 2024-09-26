#! /bin/bash
cargo build
cd test/project_fsharp
pwd
rm -rf ./build/dev/fsharp
../../target/debug/gleam build --target fsharp
cd -
