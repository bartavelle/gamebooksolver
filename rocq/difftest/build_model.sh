#!/bin/bash
# Extracts the Rocq model to OCaml and compiles it in build/, with the Rust runner that knows the chapters.
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
ROCQ=$HERE/..
B=$HERE/build
mkdir -p "$B"

cd "$ROCQ"
python3 gen_books.py
for f in base/order base/mps base/lists base/proba model/chapters model/character model/hits model/combat model/rules \
    books/book01 books/book02 books/book03 books/book04 books/book05; do
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
