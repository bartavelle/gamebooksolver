import os
import sys
import re
import subprocess

# book 05 stuff:
# flags:
#  FoughtElix
#  GenSpecial 2  -- fire sphere
#  GenSpecial 5  -- prism from book 3
#  GenBackpack 0 -- rope
#  GenSpecial 9  -- blue stone triangle
#  GenSpecial 11 -- onyx medallion

alldisciplines = set({"CA", "HU", "6S", "TR", "HL", "MS", "MB", "AK", "MO"})
allweapons = set({"DA", "SP", "MA", "WH", "SW", "AX", "QS", "SO", "SS"})
PTRN = re.compile(r"data/B(\d\d)/(\d\d)(\d\d)(..(?:\...)*)(g\d+)?(?:-(.+))?\.solutions\.zstd")

path = sys.argv[1]

m = PTRN.match(path)
assert m is not None, path
(b, e, s, sdiscs, mgold, items) = m.groups()
discs = sdiscs.split(".")

stem = path.removesuffix(".solutions.zstd")
cmdline = [
    "target/release/gamebooksolver-explorer",
    "--solpath",
    path,
    "--desc",
    stem + ".desc",
    "--jot",
    stem + ".jot",
    "--json",
    stem + ".json",
    "--decisions",
    stem + ".decisions.jsonl.zst",
    "solve",
]

if int(b) < 5:
    cmdline += ["--results", "data/B%02d" % (int(b) + 1,)]

cmdline += [
    "--book",
    b,
    "--maxendurance",
    e,
    "--skill",
    s,
    "--bookpath",
    "json-chapters/book%s.json" % b,
]

for d in discs:
    if d in alldisciplines:
        alldisciplines.remove(d)
    else:
        cmdline += ["-d", d]
for d in alldisciplines:
    cmdline += ["-d", d]

if items:
    for i in items.split("-"):
        if i.startswith("FLG"):
            cmdline += ["--flag", i[3:]]
        else:
            cmdline += ["-i", i]
if mgold:
    cmdline += ["--gold", mgold[1:]]
elif int(b) == 5:
    # gold does not cross from Book 4, and above 20 crowns it has not changed a score: see G4 in ASSUMPTIONS.md
    cmdline += ["--gold", "50"]
else:
    cmdline += ["--gold", "15"]

# cmdline += [sys.argv[1], "+RTS", "-N1", "-t", "-M110G"]
print(" ".join(cmdline))
try:
    commit = subprocess.run(["git", "rev-parse", "--short", "HEAD"], capture_output=True, text=True).stdout.strip()
    r = subprocess.run(cmdline, check=True, capture_output=True, env={**os.environ, "GBS_COMMIT": commit})
    if r.stdout:
        print("e:%s s:%s ds:%s r:%s" % (e, s, discs, r.stdout.decode("utf-8").strip()))
    if r.stderr:
        print("e:%s s:%s ds:%s r:%s" % (e, s, discs, r.stderr.decode("utf-8").strip()))
except subprocess.CalledProcessError as e:
    print(e.stderr.decode("utf-8"))
    raise
