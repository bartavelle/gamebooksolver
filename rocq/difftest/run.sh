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

cd "$ROCQ"
python3 gen_books.py
for f in mps proba chapters character hits combat rules book01 book02 book03 book04 book05; do
  rocq compile -w none -R . LoneWolfProofs "$f.v"
done
rocq compile -w none -R . LoneWolfProofs -R difftest Difftest difftest/difft.v

cd "$HERE"
cargo build --release --manifest-path rust/Cargo.toml --target-dir "$B/target"
RUNNER=$B/target/release/difftest

cd "$B"
cp "$HERE/cw.ml" "$HERE/main.ml" .
python3 "$HERE/gen_names.py"
ocamlfind ocamlopt -w -a -c cw.ml
ocamlfind ocamlopt -w -a -c model.mli
ocamlfind ocamlopt -w -a -inline 0 -c model.ml
ocamlfind ocamlopt -w -a -c names.ml
ocamlfind ocamlopt -w -a -c main.ml
ocamlfind ocamlopt -w -a cw.cmx model.cmx names.cmx main.cmx -o rocqrun

python3 "$HERE/gencases.py" "$ROOT" "$RUNNER" "$PER_CHAPTER" "$SEED" cases.txt
./rocqrun cases.txt > rocq.txt
"$RUNNER" cases.txt > rust.txt
python3 "$HERE/compare.py" cases.txt rust.txt rocq.txt 3
