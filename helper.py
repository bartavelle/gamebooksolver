from fractions import Fraction
from typing import Dict, Iterable, Iterator, List, Tuple
import json
import os


def chunks(l: List[str], n: int) -> Iterable[List[str]]:
    n = max(1, n)
    return (l[i : i + n] for i in range(0, len(l), n))


def load_jsons(dir: str, ignored: List[str]) -> Dict[str, Tuple[Fraction, int]]:
    o: Dict[str, Tuple[Fraction, int]] = {}
    for f in os.listdir(dir):
        if not f.endswith(".json"):
            continue
        ignore = False
        for i in ignored:
            if i in f:
                ignore = True
                break
        if ignore:
            continue
        fn = os.path.join(dir, f)
        j = json.load(open(fn, "r"))
        entry = j["_msentries"][0]
        r = entry["_mratio"]
        c = entry["_states"]
        o[fn] = (Fraction(r["numerator"], r["denominator"]), c)
    return o


def json_groups(ls: Dict[str, Tuple[Fraction, int]]) -> Dict[Fraction, List[str]]:
    o: Dict[Fraction, List[Tuple[str, int]]] = {}
    for k, (v, stts) in ls.items():
        if v in o:
            o[v].append((k, stts))
        else:
            o[v] = [(k, stts)]
    ro: Dict[Fraction, List[str]] = {}
    for k, v in o.items():
        v.sort(key=lambda a: a[1])
        ro[k] = [x[0] for x in v]
    return ro


def extra_builds(d: Dict[Fraction, List[str]], options: List[str]):
    print("#!/bin/sh")
    print("# %d items, %d options" % (len(d), len(options)))
    for o in options:
        print("# " + o)
        mcbor: List[str] = []
        for grp in d.values():
            aheadjson = grp[0]
            aheadraw = aheadjson[:-10]
            mcbor.append(aheadraw + "-" + o + ".cbor")

        for g in chunks(mcbor, 16):
            print("make -j2 " + " ".join(g))
            print("./backup.sh &")

        for p, grp in d.items():
            print("# %f" % (float(p),))
            headjson = grp[0]
            headraw = headjson[:-10]
            nxt = grp[1:]
            cbor = headraw + "-" + o + ".cbor"
            json = cbor + ".json"
            for n in nxt:
                nraw = n[:-10]
                ncbor = nraw + "-" + o + ".cbor"
                njson = ncbor + ".json"
                print("touch " + ncbor)
                print("cp " + json + " " + njson)


def adjust_jsons(dir: str, options: List[str]):
    for f in os.listdir(dir):
        if not f.endswith(".json"):
            continue
        if any(o in f for o in options):
            continue
        fullpath = os.path.join(dir, f)
        orig_content = json.load(open(fullpath, "r"))
        for o in options:
            newpath = fullpath[:-10] + "-" + o + ".cbor.json"
            new_content = json.load(open(newpath, "r"))
            new_content["_msdisciplines"] = orig_content["_msdisciplines"]
            new_content["_variable"]["_cvflags"].append("PermanentSkillReduction2")
            json.dump(new_content, open(newpath, "w"))


B03BASE = [
    "-Sommerswerd-Laumspur-Meal-Meal",
    "-Sommerswerd-BodyArmor-Meal-Meal",
    "-Sommerswerd-Laumspur-BodyArmor",
]

B04OPTIONS = ["Helmet-FLGHelmetIsSilver", "StrengthPotion4"]
B04BASE = [
    "g0-Sword-Dagger-Laumspur-Laumspur-Shield-BodyArmor-Meal-Meal-Meal-Meal-Meal",
    "g0-Sommerswerd-Sword-Laumspur-Laumspur-Shield-BodyArmor-Meal-Meal-Meal-Meal-Meal",
]

B05OPTIONS = ["Helmet-FLGHelmetIsSilver", "StrengthPotion4", "FLGFoughtElix", "BodyArmor"]
B05BASE= [
    "-Sword-Meal-Meal-Shield-Laumspur",
    "-Sommerswerd-Meal-Meal-Shield-Laumspur",
]

def combinations(l: List[str], acc: List[str]) -> Iterator[List[str]]:
    if not l:
        yield acc
    else:
        e = l[0]
        l2 = l[1:]
        yield from combinations(l2, acc)
        acc2 = acc + [e]
        yield from combinations(l2, acc2)


def geneqps(base: List[str], options: List[str]) -> Iterator[str]:
    for b in base:
        for o in combinations(options, []):
            yield "-".join([b] + o)


DISCS = {"CA", "HU", "6S", "TR", "HL", "MS", "MB", "AK", "MO"}
RDISCS = {"CA", "HU", "6S", "TR", "MS", "MO"}

TGTS3: List[str] = []
for t in B03BASE:
    for d1 in RDISCS:
        for d2 in RDISCS:
            if d1 > d2:
                for d3 in RDISCS:
                    if d2 > d3:
                        TGTS3.append("data/B03/2010SW.%s.%s.%s%s.cbor" % (d1, d2, d3, t))

TGTS4: List[str] = []
for d1 in RDISCS:
    for d2 in RDISCS:
        if d1 > d2:
            for t in geneqps(B04BASE, B04OPTIONS):
                TGTS4.append("data/B04/2010SW.%s.%s%s.cbor" % (d1, d2, t))

TGTS5: List[str] = []
for d1 in RDISCS:
    for t in geneqps(B05BASE, B05OPTIONS):
        TGTS5.append("data/B05/2010SW.%s%s.cbor" % (d1, t))


def chunked_makefile(n: int, l: List[str], cs: int):
    print("# LOW%02d : %d" % (n, len(l)))
    lc = list(chunks(l, cs))
    for (i, l) in enumerate(lc):
        print("LOW%02d_%d = %s" % (n, i, " ".join(l)))
        print("")
        print("low%02d_%d: $(LOW%02d_%d)" % (n, i, n ,i))
        print("")
    print("TARGETSLOWB%02d = %s" % (n, " ".join("$(LOW%02d_%d)" % (n, i) for i in range(0, len(lc)))))
    print("")


chunked_makefile(3, TGTS3, 60)
chunked_makefile(4, TGTS4, 30)
chunked_makefile(5, TGTS5, 30)

opts = ["FLGPermanentSkillReduction2"]

jsons04 = load_jsons("data/B04", opts)
groups04 = json_groups(jsons04)
# extra_builds(groups04, ["FLGPermanentSkillReduction2"])
jsons05 = load_jsons("data/B05", opts)
groups05 = json_groups(jsons05)
# extra_builds(groups05, ["FLGPermanentSkillReduction2"])
# adjust_jsons("data/B05", opts)
