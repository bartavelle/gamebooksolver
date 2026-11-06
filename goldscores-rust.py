"""Solves a character once for a range of starting gold.

goldscores-rust.py data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.goldranges
    writes the solution of all the gold to data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.solutions.zstd, the score of each
    gold to data/B02/2010SW.TR.MS.CA.6Sg<gold>-Sword-Shield.json, and to the given file the ranges of consecutive gold that
    have the same score.
goldscores-rust.py --reports data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.reports 10,20,50
    writes the .jot, .desc and .dot of these gold (or all), from the solution of the character, then the given file.
goldscores-rust.py --decisions data/B02/2010SW.TR.MS.CA.6Sg10_50-Sword-Shield.decisions 10,20,50
    solves again these gold (or all), and writes the value of every choice on the best path of each to
    data/B02/2010SW.TR.MS.CA.6Sg<gold>-Sword-Shield.decisions.jsonl.zst, then the given file.
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
    stem = path.removesuffix(".reports")
    b = re.match(r"data/B(\d\d)/", path)[1]
    # a character with a range of gold has a report for each, and the others only have one
    per_gold = re.sub(r"g\d+_\d+", "g{gold}", stem, count=1)
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


def character(m):
    """The arguments that describe the character of a .goldranges path, after the name of the command."""
    b, e, s, sdiscs, gfrom, gto, items = m.groups()
    cmdline = []
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
    return cmdline


def run(cmdline):
    commit = subprocess.run(["git", "rev-parse", "--short", "HEAD"], capture_output=True, text=True).stdout.strip()
    subprocess.run(cmdline, check=True, stdout=subprocess.DEVNULL, env={**os.environ, "GBS_COMMIT": commit})


def solve(path):
    m = PTRN.fullmatch(path)
    assert m is not None, path
    gfrom, gto = m.group(5), m.group(6)
    run(
        [
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
        + character(m)
    )


def decisions(path, golds):
    """Solves again each of these gold (or all), writing the value of every choice on the best path, then the given file."""
    stem = path.removesuffix(".decisions")
    m = PTRN.fullmatch(stem + ".goldranges")
    assert m is not None, path
    gfrom, gto = int(m.group(5)), int(m.group(6))
    wanted = range(gfrom, gto + 1) if golds == "all" else [int(g) for g in golds.split(",")]
    for gold in wanted:
        per_gold = re.sub(r"g\d+_\d+", "g%d" % gold, stem, count=1)
        run(
            ["target/release/gamebooksolver-explorer", "--solpath", per_gold + ".decisions.jsonl.zst", "decisions", "--gold", str(gold)]
            + character(m)
        )
    open(path, "w").write(golds + "\n")


if sys.argv[1] == "--summary":
    summary(sys.argv[2])
elif sys.argv[1] == "--reports":
    reports(sys.argv[2], sys.argv[3])
elif sys.argv[1] == "--decisions":
    decisions(sys.argv[2], sys.argv[3])
else:
    solve(sys.argv[1])
