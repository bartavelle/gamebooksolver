"""Solves a character once for a range of starting gold.

goldscores-rust.py data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.goldranges
    writes the solution of all the gold to data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.solutions.zstd, the score of each
    gold to data/B02/2010SW.TR.MS.CA.6Sg<gold>-Sword-Shield.json, and to the given file the ranges of consecutive gold that
    have the same score.
goldscores-rust.py --reports data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.reports 10,20,50
    writes the .jot, .desc and .dot of these gold (or all), from the solution of the character, then the given file.
goldscores-rust.py --summary data/B02
    shows these ranges for all the characters that were solved.
"""
import glob
import json
import os
import re
import subprocess
import sys

alldisciplines = {"CA", "HU", "6S", "TR", "HL", "MS", "MB", "AK", "MO"}
PTRN = re.compile(r"data/B(\d\d)/(\d\d)(\d\d)(..(?:\...)*)g(\d+)_(\d+)(?:-(.+))?\.goldranges")


def summary(directory):
    for path in sorted(glob.glob(directory + "/*.goldranges")):
        ranges = json.load(open(path))
        name = path.split("/")[-1].removesuffix(".goldranges")
        parts = ["%d-%d %.3f%%" % (r["from"], r["to"], r["score"] * 100) for r in ranges]
        print("%s: %d scores: %s" % (name, len(ranges), ", ".join(parts)))


def reports(path, golds):
    m = re.fullmatch(r"(data/B(\d\d)/.*?)g\d+_\d+((?:-.+)?)\.reports", path)
    assert m is not None, path
    prefix, b, items = m.groups()
    stem = path.removesuffix(".reports")
    per_gold = prefix + "g{gold}" + items
    subprocess.run(
        [
            "target/release/gamebooksolver-explorer",
            "--solpath",
            stem + ".solutions.zstd",
            "--jot",
            per_gold + ".jot",
            "--desc",
            per_gold + ".desc",
            "extract",
            "--golds",
            golds,
            "--dot",
            per_gold + ".dot",
            "--bookpath",
            "json-chapters/book%s.json" % b,
            "--display",
            "json-chapters/display%s.json" % b,
        ],
        check=True,
    )
    open(path, "w").write(golds + "\n")


def solve(path):
    m = PTRN.fullmatch(path)
    assert m is not None, path
    b, e, s, sdiscs, gfrom, gto, items = m.groups()
    cmdline = [
        "target/release/gamebooksolver-explorer",
        "--solpath",
        path.removesuffix(".goldranges") + ".solutions.zstd",
        "--json",
        re.sub(r"g\d+_\d+", "g{gold}", path.removesuffix(".goldranges"), count=1) + ".json",
        "gold-scores",
        "--ranges",
        path,
        "--gold",
        gfrom,
        "--gold-to",
        gto,
    ]
    if int(b) < 5:
        cmdline += ["--results", "data/B%02d" % (int(b) + 1,)]
    cmdline += ["--book", b, "--maxendurance", e, "--skill", s, "--bookpath", "json-chapters/book%s.json" % b]
    missing = set(alldisciplines)
    for d in sdiscs.split("."):
        if d in missing:
            missing.remove(d)
        else:
            cmdline += ["-d", d]
    for d in missing:
        cmdline += ["-d", d]
    for i in (items or "").split("-"):
        if i.startswith("FLG"):
            cmdline += ["--flag", i[3:]]
        elif i:
            cmdline += ["-i", i]
    commit = subprocess.run(["git", "rev-parse", "--short", "HEAD"], capture_output=True, text=True).stdout.strip()
    subprocess.run(cmdline, check=True, stdout=subprocess.DEVNULL, env={**os.environ, "GBS_COMMIT": commit})


if sys.argv[1] == "--summary":
    summary(sys.argv[2])
elif sys.argv[1] == "--reports":
    reports(sys.argv[2], sys.argv[3])
else:
    solve(sys.argv[1])
