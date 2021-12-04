import sys
import re
import subprocess

PTRN = re.compile("data/(\d\d)(\d\d)(..)(?:\.(..))?\.cbor")

m = PTRN.match(sys.argv[1])
(e, s, d1, d2) = m.groups()

cmdline = ["./dist/gamebooksolver-solvebook02", "soldump", "--mode", "cbor", "-e", e, "-s", s, "-d", d1]
if d2:
    cmdline += ["-d", d2]
cmdline += [sys.argv[1], "+RTS", "-N1"]
r = subprocess.run(cmdline, check=True, capture_output=True)
if r.stdout:
    print(r.stdout.decode("utf-8"))
if r.stderr:
    print(r.stderr.decode("utf-8"))
