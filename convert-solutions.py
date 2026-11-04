"""Converts the solutions of the previous format, data/B0x/*.bin.zstd, to data/B0x/*.solutions.zstd.

Each file is checked by reading it back. The files of the previous format are left as they are.

convert-solutions.py [number of jobs] data/B03 data/B04 data/B05
"""
import glob
import os
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor

EXPLORER = os.environ.get("EXPLORER", "target/release/gamebooksolver-explorer")


def convert(source):
    target = source.removesuffix(".bin.zstd") + ".solutions.zstd"
    if os.path.exists(target):
        return source, "skipped", 0, os.path.getsize(target)
    r = subprocess.run([EXPLORER, "--solpath", source, "convert-solution", target], capture_output=True, text=True)
    if r.returncode != 0:
        return source, "FAILED: " + r.stderr[-300:], os.path.getsize(source), 0
    return source, "ok", os.path.getsize(source), os.path.getsize(target)


jobs = int(sys.argv[1])
sources = [f for d in sys.argv[2:] for f in sorted(glob.glob(d + "/*.bin.zstd"))]
counts = {}
before = after = 0
with ThreadPoolExecutor(jobs) as pool:
    for source, status, old, new in pool.map(convert, sources):
        key = status.split(":")[0]
        counts[key] = counts.get(key, 0) + 1
        if key == "FAILED":
            print(source, status)
        before += old
        after += new
print(counts, "from", before, "bytes to", after, "bytes")
sys.exit(1 if "FAILED" in counts else 0)
