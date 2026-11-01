#!/usr/bin/env python3
"""Generates the bookNN.v files from the books used by the Rust solver: json-chapters/bookNN.json."""
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
WEAPONS = {"Dagger", "Spear", "Mace", "ShortSword", "Warhammer", "Sword", "Axe", "Quarterstaff", "BroadSword", "MagicSpear", "Sommerswerd"}
SLOTS = {"WeaponSlot": "SWeapon", "BackpackSlot": "SBackpack", "SpecialSlot": "SSpecial", "PouchSlot": "SPouch"}


def nat(n):
    return f"{n}%nat"


def item(i):
    if i in WEAPONS:
        return f"(Weapon {i})"
    if i == "SilverHelmet":
        return "(GenSpecial S10)"
    if i.startswith("SGenCounter "):
        return f"(GenSpecial S{i.split()[1]})"
    if i.startswith("BGenCounter "):
        return f"(GenBackpack S{i.split()[1]})"
    return i


def proba(p):
    num, _, den = p["value"].partition("/")
    return f"(Q2Qc ({num} / {den or 1})%Q)"


def rlist(lst):
    return "[" + "; ".join(lst) + "]"


def fight_mod(m):
    if isinstance(m, str):
        return "Dpr" if m == "DPR" else m
    (k, v), = m.items()
    if k == "Timed":
        return f"Timed {nat(v[0])} ({fight_mod(v[1])})"
    if k == "Poisonous":
        return f"Poisonous {proba(v)}"
    if k == "CombatBonus":
        return f"CombatBonus ({v})%Z"
    if k == "DPR":
        return f"Dpr {nat(v)}"
    return f"{k} {nat(v)}"


def details(fd):
    skill = fd["fcombatSkill"]
    return f"Details (({skill})%Z)%Z {nat(fd['fendurance'])} {rlist([fight_mod(m) for m in fd['fightMod']])}"


def simple(so):
    if isinstance(so, str):
        return so
    (k, v), = so.items()
    if k in ("GainItem", "LoseItem"):
        return f"{k} {item(v[0])} {nat(v[1])}"
    if k == "LoseItemKind":
        return f"{k} {rlist([SLOTS[s] for s in v])}"
    if k in ("DamagePlayer", "HealPlayer"):
        return f"{k} {nat(v)}"
    return f"{k} {v}"


def cond(c):
    (k, v), = c.items()
    if k == "HasDiscipline":
        return f"HasDiscipline ({discipline(v)})"
    if k == "Not":
        return f"Not ({cond(v)})"
    if k in ("COr", "CAnd"):
        return f"{k} ({cond(v[0])}) ({cond(v[1])})"
    if k == "HasItem":
        return f"HasItem {item(v[0])} {nat(v[1])}"
    if k == "Always":
        return "Always " + ("true" if v else "false")
    if k == "HasEndurance":
        return f"HasEndurance {nat(v)}"
    return f"{k} {v}"


def discipline(d):
    if d in WEAPONS:
        return f"WeaponSkill {d}"
    return d


def outcome(co):
    if isinstance(co, str):
        return co
    (k, v), = co.items()
    if k == "Fight":
        return f"Fight ({details(v[0])}) ({outcome(v[1])})"
    if k == "OneRound":
        return f"OneRound ({details(v[0])}) ({outcome(v[1])}) ({outcome(v[2])}) ({outcome(v[3])})"
    if k == "Randomly":
        return "Randomly " + rlist([f"({outcome(o)} , {proba(p)})" for p, o in v])
    if k == "LoseItemFrom":
        return f"LoseItemFrom {SLOTS[v[0]]} {nat(v[1])} ({outcome(v[2])})"
    if k == "Conditionally":
        return "Conditionally " + rlist([f"({cond(c)} , {outcome(o)})" for c, o in v])
    if k == "Simple":
        return f"Simple {rlist([simple(s) for s in v[0]])} ({outcome(v[1])})"
    if k == "Goto":
        return f"Goto {nat(v)}"
    raise ValueError(k)


def decision(d):
    (k, v), = d.items()
    if k == "Decisions":
        return "Decisions " + rlist([decision(x[1]) for x in v])
    if k == "RetrieveEquipment":
        return f"retrieve_equipment ({decision(v)})"
    if k == "CanTake":
        return f"can_take {item(v[0])} {nat(v[1])} ({decision(v[2])})"
    if k == "Canbuy":
        return f"can_buy {item(v[0])} {nat(v[1])} ({decision(v[2])})"
    if k == "Cansell":
        return f"can_sell {item(v[0])} {nat(v[1])} ({decision(v[2])})"
    if k == "Conditional":
        return f"conditional ({cond(v[0])}) ({decision(v[1])})"
    if k == "Special":
        return f"(special {v})"
    if k == "NoDecision":
        return f"none ({outcome(v)})"
    if k == "EvadeFight":
        return f"evade_fight {nat(v[0])} {nat(v[1])} ({details(v[2])}) ({outcome(v[3])})"
    if k == "AfterCombat":
        return f"after_combat ({decision(v)})"
    if k == "RemoveItemFrom":
        return f"remove_item_from {SLOTS[v[0]]} {nat(v[1])} ({decision(v[2])})"
    raise ValueError(k)


def book(n):
    with open(os.path.join(HERE, "..", "json-chapters", f"book0{n}.json"), encoding="utf-8") as f:
        chapters = json.load(f)
    body = ";\n  ".join(f"({nat(c)},{decision(ch['pchoice'])})" for c, ch in chapters)
    return (
        "Require Import chapters.\nRequire Import Stdlib.QArith.Qcanon.\nRequire Import Stdlib.QArith.QArith.\n"
        "Require Import Stdlib.Lists.List.\nImport ListNotations.\n"
        f"Definition chapters := [{body}].\n"
    )


if __name__ == "__main__":
    out = sys.argv[1] if len(sys.argv) > 1 else HERE
    for n in range(1, 6):
        with open(os.path.join(out, f"book0{n}.v"), "w", encoding="utf-8") as f:
            f.write(book(n))
