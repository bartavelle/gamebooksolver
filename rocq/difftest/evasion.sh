#!/bin/bash
# Checks the fights of Book 4 that can be evaded against a separate calculation, see evasion.py.
# usage: evasion.sh [options of evasion.py]
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
cd "$HERE/../.."
cargo build --release -q --example evasion_score -p gamebooksolver-explorer
exec python3 "$HERE/evasion.py" target/release/examples/evasion_score "$@"
