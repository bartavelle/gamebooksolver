#!/bin/bash

F1="$1"
F2="$2"
CID="$3"

./target/release/gamebooksolver-explorer -s "$F1" dump-states "$CID" | sed -e 's/[0-9]\+\.[0-9]\+% -//' | sort > /tmp/sorted_f1
./target/release/gamebooksolver-explorer -s "$F2" dump-states "$CID" | sed -e 's/[0-9]\+\.[0-9]\+% -//' | sort > /tmp/sorted_f2

diff -u /tmp/sorted_f1 /tmp/sorted_f2
