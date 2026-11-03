#!/bin/bash
# Checks what ValidState says on the five books, on the extracted model. It is a test, as playing keeping it true isn't proved.
# usage: valid.sh [number of cases per chapter] [number of random games per book] [seed]
set -e
PER_CHAPTER=${1:-12}
GAMES=${2:-300}
SEED=${3:-1}
HERE=$(cd "$(dirname "$0")" && pwd)
ROOT=$HERE/../..
B=$HERE/build

"$HERE/build_model.sh"
cd "$B"
cp "$HERE/valid.ml" .
ocamlfind ocamlopt -w -a -c valid.ml
ocamlfind ocamlopt -w -a cw.cmx model.cmx names.cmx valid.cmx -o validrun
python3 "$HERE/gen_valid.py" "$ROOT" "$B/target/release/difftest" "$PER_CHAPTER" "$GAMES" "$SEED" valid_cases.txt
./validrun valid_cases.txt
