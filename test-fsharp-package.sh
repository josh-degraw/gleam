#! /bin/bash
# Check if an argument is provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <project_path>"
    exit 1
fi

PROJECT_PATH="$1"
ROOT_PATH=$(pwd)

cargo build && \
cd "$PROJECT_PATH" && \
pwd && \
# rm -rf ./build/dev/fsharp && \
"$ROOT_PATH"/target/debug/gleam build --target fsharp && \
cd -
