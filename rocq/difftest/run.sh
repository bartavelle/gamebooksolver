#!/bin/bash
# Checks that the Rocq model behaves like the Rust solver. The model is extracted to OCaml, then both are run on random
# characters at each chapter of each book, and all the choices and their results must be the same.
# usage: run.sh [number of cases per chapter] [seed]
set -e
PER_CHAPTER=${1:-4}
SEED=${2:-1}
HERE=$(cd "$(dirname "$0")" && pwd)
ROCQ=$HERE/..
ROOT=$HERE/../..
B=$HERE/build
mkdir -p "$B"

"$HERE/build_model.sh"
RUNNER=$B/target/release/difftest
cd "$B"
python3 "$HERE/gencases.py" "$ROOT" "$RUNNER" "$PER_CHAPTER" "$SEED" cases.txt
./rocqrun cases.txt > rocq.txt
"$RUNNER" cases.txt > rust.txt
python3 "$HERE/compare.py" cases.txt rust.txt rocq.txt 3
