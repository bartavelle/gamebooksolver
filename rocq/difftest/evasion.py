#!/usr/bin/env python3
"""Compares the solver with a separate calculation of the fights of Book 4 that can be evaded.

The combat table comes from the XML of the book and the fights from json-chapters/book04.json. What comes after a fight
gets a random value for each Endurance, so that evading is a real choice. No code is shared with the solver.

evasion.py SCORER [--free-evasion] [--limit N]
--free-evasion breaks this calculation on purpose (no round is played when evading), to show that a mistake is found.
"""
import argparse
import html
import json
import random
import re
import subprocess
import sys
from concurrent.futures import ThreadPoolExecutor
from fractions import Fraction
from functools import lru_cache

KILL = 1000
FOLLOWUP = {"fcombatSkill": 15, "fendurance": 12, "fightMod": [], "opponent": "Next"}
# discipline names, weapons and other items; a character of Book 4 has seven disciplines and a Weapon Skill
ALL = ["Camouflage", "Hunting", "SixthSense", "Tracking", "Healing", "MindShield", "MindBlast", "MindOverMatter", "AnimalKinship"]
KITS = [
    ("no MS CA, Sword Dagger", ["MindShield", "Camouflage"], "SW", ["Sword", "Dagger", "Shield", "BodyArmor", "Laumspur"]),
    ("no TR MO, Sword Dagger", ["Tracking", "MindOverMatter"], "SW", ["Sword", "Dagger", "Shield", "BodyArmor", "Laumspur"]),
    ("no TR HU, Sommerswerd", ["Tracking", "Hunting"], "SW", ["Sommerswerd", "Sword", "Shield", "BodyArmor", "SilverHelmet", "StrengthPotion4"]),
    ("no MB HL, Dagger only", ["MindBlast", "Healing"], "SW", ["Dagger", "StrengthPotion4"]),
    ("no MB MS, Skill Dagger", ["MindBlast", "MindShield"], "Dagger", ["Dagger", "Sword", "StrengthPotion"]),
    ("no HL TR, no weapon", ["Healing", "Tracking"], "SW", []),
]
FIGHTS = [14, 77, 108, 133, 138, 186, 202, 316, 362]
HPS = [3, 7, 12, 16, 20]
VARIANTS = ["table", "followup", "goto"]


def combat_table(path):
    """{ratio column: [(enemy loss, Lone Wolf loss) for the random numbers 0 to 9]}, and the first ratio of each column"""
    text = open(path, encoding="utf-8").read().replace("<ch.minus/>", "-")
    start = text.index('id="crtable"')
    body = text[text.index("<tbody>", start) : text.index("</table>", start)]
    rows = [re.findall(r"<t[dh][^>]*>(.*?)</t[dh]>", r, re.S) for r in re.findall(r"<tr>(.*?)</tr>", body, re.S)]
    heads = [re.findall(r"-?\d+", h) for h in rows[1]]
    tops = [int(h[-1]) for h in heads]
    assert len(tops) == 13 and tops == sorted(tops) and len(rows) == 12
    table = [{} for _ in tops]
    for row in rows[2:]:
        for col, cell in enumerate(row[1:]):
            op, lw = cell.split("/")
            table[col][int(row[0])] = (KILL if op == "k" else int(op), KILL if lw == "k" else int(lw))
    return tops, [[t[n] for n in range(10)] for t in table]


def column(tops, ratio):
    for n, top in enumerate(tops[:-1]):
        if ratio <= top:
            return n
    return len(tops) - 1


class Calculation:
    def __init__(self, tops, table, chapters, case, free_evasion=False, evasion=True):
        self.tops, self.table, self.case, self.free_evasion, self.evasion = tops, table, case, free_evasion, evasion
        self.chain = fight_chain(chapters, case["start"])
        self.max_hp = case["maxend"] + (4 if "BodyArmor" in case["items"] else 0)
        self.potion = 4 if "StrengthPotion4" in case["items"] else 2 if "StrengthPotion" in case["items"] else 0
        self.tables = case["tables"]
        self.value_fight = lru_cache(maxsize=None)(self.fight)
        self.value_follow = lru_cache(maxsize=None)(self.follow)

    def ratio(self, fd, active):
        c, items = self.case, self.case["items"]
        mods = fd["fightMod"]
        r = c["skill"] - fd["fcombatSkill"] + active
        r += sum(m["CombatBonus"] for m in mods if isinstance(m, dict) and "CombatBonus" in m)
        if "Sommerswerd" in items:
            r += 10 if "SW" in c["disc"] else 8
        elif not any(w in items for w in ("Sword", "Dagger")):
            r -= 4
        elif ("SW" in c["disc"] and "Sword" in items) or ("Dagger" in c["disc"] and "Dagger" in items):
            r += 2
        if "MindBlast" in c["disc"] and "MindblastImmune" not in mods:
            r += 2
        if "Shield" in items:
            r += 2
        if "SilverHelmet" in items:
            r += 2
        return r

    def losses(self, fd, active):
        for op, lw in self.table[column(self.tops, self.ratio(fd, active))]:
            if "WeakMindblast" in fd["fightMod"] and "MindShield" not in self.case["disc"]:
                lw += 1
            yield op, lw

    def after(self, kind, cid, hp, avail):
        """the value of arriving at a chapter that was replaced"""
        variant = self.case["variant"]
        if kind == "damage" or variant == "table":
            return self.tables[str(cid)][hp]
        if variant == "goto":
            if cid < 350 and "Healing" in self.case["disc"]:
                hp = min(hp + 1, self.max_hp)
            return self.tables[str(cid)][hp]
        return self.enter_follow(cid, hp, avail)

    def enter_follow(self, cid, hp, avail):
        best = self.value_follow(cid, FOLLOWUP["fendurance"], hp, 0)
        if avail:
            best = max(best, self.value_follow(cid, FOLLOWUP["fendurance"], hp, avail))
        return best

    def follow(self, cid, ep, hp, active):
        total = Fraction(0)
        for op, lw in self.losses(FOLLOWUP, active):
            nhp, nep = hp - lw, ep - op
            if nhp <= 0:
                continue
            if nep <= 0:
                total += self.tables[str(cid)][nhp]
            else:
                total += self.value_follow(cid, nep, nhp, active)
        return total / 10

    def enter(self, i, hp, avail, active):
        ep = self.chain[i]["fd"]["fendurance"]
        best = self.value_fight(i, 0, ep, hp, avail, active)
        if avail:
            best = max(best, self.value_fight(i, 0, ep, hp, 0, avail))
        return best

    def fight(self, i, rnd, ep, hp, avail, active):
        f = self.chain[i]
        fd = f["fd"]
        ondamage = next((m["OnDamage"] for m in fd["fightMod"] if isinstance(m, dict) and "OnDamage" in m), None)
        on = Fraction(0)
        away = Fraction(0)
        for op, lw in self.losses(fd, active):
            nhp, nep = hp - lw, ep - op
            if nhp <= 0:
                continue
            if ondamage is not None and nhp < hp:
                value = self.after("damage", ondamage, nhp, avail)
                on += value
                away += value
                continue
            away += self.after("evaded", f["evade"], nhp, avail)
            if nep > 0:
                on += self.value_fight(i, min(rnd + 1, f["rounds"]), nep, nhp, avail, active)
            elif i + 1 < len(self.chain):
                on += self.enter(i + 1, nhp, avail, active if "MultiFight" in fd["fightMod"] else 0)
            else:
                on += self.after("won", f["won"], nhp, avail)
        on /= 10
        away /= 10
        if self.free_evasion:
            away = self.after("evaded", f["evade"], hp, avail)
        return on if rnd < f["rounds"] or not self.evasion else max(on, away)

    def score(self):
        return self.enter(0, self.case["hp"], self.potion, 0)


def fight_chain(chapters, start):
    """the fights that follow each other, as in section 202"""
    chain = []
    cid = start
    while True:
        rounds, evade, fd, after = chapters[cid]["pchoice"]["EvadeFight"]
        target = after["Simple"][1]["Goto"] if "Simple" in after else after["Goto"]
        chain.append({"cid": cid, "rounds": rounds, "evade": evade, "fd": fd, "won": target})
        if target in chapters and "EvadeFight" in chapters[target]["pchoice"]:
            cid = target
        else:
            return chain


def chapter(pchoice):
    return {"title": "", "desc": "", "pchoice": pchoice}


def value_chapter(values, max_hp):
    def ending(p):
        if p == 0:
            return "GameLost"
        if p == 1:
            return "GameWon"
        return {"Randomly": [[frac(p), "GameWon"], [frac(1 - p), "GameLost"]]}

    rows = [[{"HasEndurance": hp}, ending(values[hp])] for hp in range(max_hp, 1, -1)]
    rows.append([{"Always": True}, ending(values[1])])
    return chapter({"NoDecision": {"Conditionally": rows}})


def frac(p):
    return {"radix": 10, "value": f"{p.numerator}/{p.denominator}"}


def random_values(rng, max_hp, shape):
    if shape == "any":
        return {hp: Fraction(rng.randint(0, 20), 20) for hp in range(0, max_hp + 2)}
    values = sorted(rng.randint(0, 20) for _ in range(max_hp + 2))
    return {hp: Fraction(values[hp], 20) for hp in range(0, max_hp + 2)}


def build_case(chapters, rng, start, kit, hp, variant, shape):
    name, missing, skill, items = kit
    disc = [d for d in ALL if d not in missing] + [skill]
    case = {"start": start, "skill": 10, "maxend": 20, "hp": hp, "disc": disc, "items": items, "variant": variant, "kit": name}
    max_hp = 24 if "BodyArmor" in items else 20
    chain = fight_chain(chapters, start)
    book = [[f["cid"], chapters[f["cid"]]] for f in chain]
    targets = {f["evade"] for f in chain} | {chain[-1]["won"]}
    tables = {}
    for cid, new in zip(sorted(targets), (430, 431, 432)):
        values = random_values(rng, max_hp, shape)
        tables[str(cid)] = values
        if variant == "table":
            book.append([cid, value_chapter(values, max_hp)])
        elif variant == "goto":
            book.append([cid, chapter({"NoDecision": {"Goto": new}})])
            book.append([new, value_chapter(values, max_hp)])
        else:
            book.append([cid, chapter({"NoDecision": {"Fight": [FOLLOWUP, {"Goto": new}]}})])
            book.append([new, value_chapter(values, max_hp)])
    for f in chain:
        for m in f["fd"]["fightMod"]:
            if isinstance(m, dict) and "OnDamage" in m:
                values = random_values(rng, max_hp, shape)
                tables[str(m["OnDamage"])] = values
                book.append([m["OnDamage"], value_chapter(values, max_hp)])
    case["tables"] = tables
    case["book"] = book
    return case


def to_fraction(m):
    a, b = (m["value"].split("/") + ["1"])[:2]
    return Fraction(int(a, m.get("radix", 10)), int(b, m.get("radix", 10)))


def run_shard(scorer, cases):
    lines = "".join(json.dumps({k: c[k] for k in ("book", "start", "skill", "maxend", "hp", "disc", "items")}) + "\n" for c in cases)
    out = subprocess.run([scorer], input=lines, capture_output=True, text=True, check=True).stdout
    return [to_fraction(json.loads(l)) for l in out.splitlines()]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("scorer")
    ap.add_argument("--free-evasion", action="store_true")
    ap.add_argument("--limit", type=int)
    ap.add_argument("--jobs", type=int, default=8)
    ap.add_argument("--xml", default="project-aon/en/xml/04tcod.xml")
    ap.add_argument("--book", default="json-chapters/book04.json")
    args = ap.parse_args()
    sys.setrecursionlimit(100000)
    tops, table = combat_table(args.xml)
    chapters = dict(json.load(open(args.book)))
    rng = random.Random(4)
    cases = [
        build_case(chapters, rng, start, kit, hp, variant, shape)
        for start in FIGHTS
        for kit in KITS
        for hp in HPS
        for variant in VARIANTS
        for shape in ("any", "rising")
    ]
    if args.limit:
        rng.shuffle(cases)
        cases = cases[: args.limit]
    shards = [cases[n :: args.jobs] for n in range(args.jobs)]
    with ThreadPoolExecutor(args.jobs) as pool:
        solved = list(pool.map(lambda s: run_shard(args.scorer, s), shards))
    bad = 0
    evading_matters = 0
    for shard, scores in zip(shards, solved):
        for case, got in zip(shard, scores):
            calc = Calculation(tops, table, chapters, case, args.free_evasion)
            want = calc.score()
            if want > Calculation(tops, table, chapters, case, evasion=False).score():
                evading_matters += 1
            if got != want:
                bad += 1
                if bad <= 10:
                    print(f"DIFF c{case['start']} {case['kit']} hp {case['hp']} {case['variant']}: solver {float(got):.8f} calculation {float(want):.8f}")
    print(f"{len(cases)} cases, {bad} differ, evading is the best choice somewhere in {evading_matters}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
