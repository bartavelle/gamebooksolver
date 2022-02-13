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
PTRN = re.compile("data/B(\d\d)/(\d\d)(\d\d)(..(?:\...)*)(g\d+)?(?:-(.+))?\.cbor")

m = PTRN.match(sys.argv[1])
assert m is not None
(b, e, s, sdiscs, mgold, items) = m.groups()
discs = sdiscs.split(".")

cmdline = ["./dist/gamebooksolver-solvebook"]

if int(b) < 5:
    cmdline += ["--results", "data/B%02d" % (int(b) + 1,)]

cmdline += ["soldump", "--book", b, "--mode", "cbor", "-e", e, "-s", s, "--autoweapon"]

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

cmdline += [sys.argv[1], "+RTS", "-N1", "-t", "-M20G"]
print(" ".join(cmdline))
try:
    r = subprocess.run(cmdline, check=True, capture_output=True)
    if r.stdout:
        print("e:%s s:%s ds:%s r:%s" % (e, s, discs, r.stdout.decode("utf-8").strip()))
    if r.stderr:
        print("e:%s s:%s ds:%s r:%s" % (e, s, discs, r.stderr.decode("utf-8").strip()))
except subprocess.CalledProcessError as e:
    print(e.stderr.decode("utf-8"))
    sys.exit(1)
