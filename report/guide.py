#!/usr/bin/env python3
"""Writes the strategy guide of a book from all its solved characters: guides/bookNN.json, and guides/bookNN.md from it.

usage: guide.py <book number>

Every character of data/B0N that has a .decisions.jsonl.zst counts the same, and each starting gold of a character of
Books 1 and 2 counts as a character. The summary written for players is read from guides/summaries/bookNN.md, and only
copied. Scores are chances of winning the series up to Book 5 when always making the best choice, as the end of each
book is worth what the solver found for the next one: see ASSUMPTIONS.md.
"""
import collections
import datetime
import glob
import json
import os
import re
import statistics
import subprocess
import sys

import analyse as A
import page as P

TITLES = {1: "Flight from the Dark", 2: "Fire on the Water", 3: "The Caverns of Kalte", 4: "The Chasm of Doom", 5: "Shadow on the Sand"}
DECISIONS = 25
DEATHS = 15
ORDER_ONLY = "Take the same items in another order"


def discipline_name(d):
    if isinstance(d, dict):
        return "Weaponskill (%s)" % d["WeaponSkill"]
    return re.sub(r"(?<=[a-z])(?=[A-Z])", " ", d).replace("Mind Shield", "Mindshield").replace("Mind Blast", "Mindblast")


def opening(book, section):
    """The first sentence of a section, so that a player can tell where it is."""
    ch = book.chapters.get(section)
    if not ch:
        return None
    text = " ".join(ch["desc"].split())
    # the sections that were added to transcribe a fight have no text of the book
    if text.startswith("Dummy"):
        return None
    return re.split(r"(?<=[.!?]) ", text)[0]


def characters(number):
    """Each solved character with its decisions file, as (description, decisions path)."""
    out = []
    for path in sorted(glob.glob(os.path.join(P.ROOT, f"data/B{number:02d}/*.decisions.jsonl.zst"))):
        stem = path.removesuffix(".decisions.jsonl.zst")
        desc = json.load(open(stem + ".json"))
        out.append((desc, path))
    return out


def number_of(desc):
    return int(desc["mbook"].removeprefix("Book"))


def study(desc, path, book, names):
    """What one character tells: its score, where it dies, and what each decision is worth."""
    root, states = A.load(path)
    chances = A.reach(root, states)
    score = A.check(root, states, chances)
    deaths = collections.Counter()
    for cid, c in A.deaths(states, chances).items():
        deaths[book.section(cid)] += c
    found = A.analyse(root, states, chances, names, None)
    decisions = {}
    for key, d in found["decisions"].items():
        where = ("fight", d["key"][1]) if key[0] == "fight" else ("section", book.section(key[1]))
        cur = decisions.setdefault(where, dict(cost=0.0, chance=0.0, worst=0.0, picks=collections.Counter(), alts=collections.Counter(), rules=[]))
        cur["cost"] += d["cost"]
        cur["chance"] += d["chance"]
        cur["worst"] = max(cur["worst"], d["worst"])
        cur["picks"].update(d["picks"])
        cur["alts"].update(d["alts"])
        if d["rule"]:
            share, test, pick_if, other = d["rule"]
            cur["rules"].append(f"{pick_if} when {test}, otherwise {other}")
    items = {names.item(k): v for k, v in (desc["variable"]["items"] or {}).items()}
    gold = desc["variable"]["gold"]
    # in Book 1, the random item can be 12 Gold Crowns, on top of the 0 to 9 of the pouch
    if number_of(desc) == 1 and not items and gold >= 12:
        items["12 Gold Crowns"] = 1
        gold -= 12
    return dict(
        name=os.path.basename(path).removesuffix(".decisions.jsonl.zst"),
        disciplines=sorted(discipline_name(d) for d in desc["msdisciplines"]),
        items=items,
        gold=gold,
        score=score,
        finish=1 - sum(deaths.values()),
        deaths=deaths,
        decisions=decisions,
    )


def summary(values):
    return dict(mean=statistics.fmean(values), median=statistics.median(values), min=min(values), max=max(values))


def compare(chars, has):
    """The mean score of the characters that have something, next to those that don't."""
    yes = [c["score"] for c in chars if has(c)]
    no = [c["score"] for c in chars if not has(c)]
    if not yes or not no:
        return None
    return dict(with_=statistics.fmean(yes), without=statistics.fmean(no), gain=statistics.fmean(yes) - statistics.fmean(no),
                characters_with=len(yes), characters_without=len(no))


def build(number):
    book = P.Book(number)
    # items keep their names from book to book, and only Book 5 has them written down
    known = json.load(open(os.path.join(P.ROOT, "report/items.json")))
    names = A.Names(known.get(str(number), known["5"]))
    chars = [study(desc, path, book, names) for desc, path in characters(number)]
    if not chars:
        raise SystemExit(f"no .decisions.jsonl.zst in data/B{number:02d}")
    n = len(chars)

    disciplines = sorted({d for c in chars for d in c["disciplines"]})
    items = sorted({i for c in chars for i in c["items"]})
    by_discipline = []
    for d in disciplines:
        cmp = compare(chars, lambda c, d=d: d in c["disciplines"])
        if cmp:
            by_discipline.append(dict(name=d, **cmp))
    by_item = []
    for i in items:
        cmp = compare(chars, lambda c, i=i: i in c["items"])
        if cmp:
            by_item.append(dict(name=i, **cmp))
    golds = sorted({c["gold"] for c in chars})
    by_gold = [dict(gold=g, mean=statistics.fmean(c["score"] for c in chars if c["gold"] == g),
                    characters=sum(1 for c in chars if c["gold"] == g)) for g in golds] if len(golds) > 1 else []

    agg = {}
    for c in chars:
        for where, d in c["decisions"].items():
            cur = agg.setdefault(where, dict(cost=0.0, chance=0.0, worst=0.0, characters=0, picks=collections.Counter(),
                                             alts=collections.Counter(), rules=collections.Counter()))
            cur["cost"] += d["cost"] / n
            cur["chance"] += d["chance"] / n
            cur["worst"] = max(cur["worst"], d["worst"])
            if d["worst"] >= A.TIE:
                cur["characters"] += 1
            # each character counts the same, whatever the chance of its reaching the decision
            total = sum(d["picks"].values())
            for k, v in d["picks"].items():
                cur["picks"][k] += v / total
            total = sum(d["alts"].values())
            for k, v in d["alts"].items():
                cur["alts"][k] += v / total
            cur["rules"].update(set(d["rules"]))
    decisions = []
    for where, d in sorted(agg.items(), key=lambda x: -x[1]["cost"]):
        if d["cost"] < A.TIE or len(decisions) == DECISIONS:
            break
        # only an order of taking the same items: nothing for a player to choose
        if all(k == ORDER_ONLY for k in d["picks"]):
            continue
        shown = sum(d["picks"].values())
        same_route = [x for x in decisions if x["kind"] == "section" == where[0] and abs(x["cost"] - d["cost"]) <= 1e-9 * d["cost"]
                      and x["characters"] == d["characters"]]
        if same_route:
            same_route[0]["sections"].append(where[1])
            same_route[0]["steps"].append(dict(section=where[1], text=opening(book, where[1]),
                                               best=[dict(choice=k, share=v / shown) for k, v in d["picks"].most_common(3)]))
            continue
        decisions.append(dict(
            kind=where[0],
            sections=[where[1]] if where[0] == "section" else [],
            opponent=where[1] if where[0] == "fight" else None,
            text=opening(book, where[1]) if where[0] == "section" else None,
            cost=d["cost"],
            # a fight is decided again at each round, so its chances add up over the rounds
            reached=d["chance"] if where[0] == "section" else None,
            worst=d["worst"],
            characters=d["characters"],
            best=[dict(choice=k, share=v / shown) for k, v in d["picks"].most_common(3) if v / shown >= 0.005],
            instead_of=[k for k, _ in d["alts"].most_common(2)],
            rules=[dict(rule=k, characters=v, internal="flag " in k) for k, v in d["rules"].most_common(3)],
            steps=[],
        ))

    deaths = collections.Counter()
    for c in chars:
        for s, v in c["deaths"].items():
            deaths[s] += v / n
    died = sum(deaths.values())
    commit = subprocess.run(["git", "rev-parse", "--short", "HEAD"], capture_output=True, text=True, cwd=P.ROOT).stdout.strip()
    summary_path = os.path.join(P.ROOT, f"guides/summaries/book{number:02d}.md")
    return dict(
        book=number,
        title=TITLES[number],
        generated=datetime.date.today().isoformat(),
        commit=commit,
        summary=open(summary_path).read() if os.path.exists(summary_path) else None,
        characters=n,
        score=summary([c["score"] for c in chars]),
        finish=summary([c["finish"] for c in chars]),
        disciplines=sorted(by_discipline, key=lambda x: -x["gain"]),
        items=sorted(by_item, key=lambda x: -x["gain"]),
        gold=by_gold,
        decisions=decisions,
        deaths=[dict(section=s, opponent=book.opponent(s), text=opening(book, s), share=v, of_deaths=v / died if died else 0) for s, v in deaths.most_common(DEATHS)],
        list=[dict(name=c["name"], disciplines=c["disciplines"], items=c["items"], gold=c["gold"], score=c["score"], finish=c["finish"])
              for c in chars],
    )


def pct(x):
    return f"{x * 100:.1f}%"


def pts(x):
    return f"{x * 100:.2f}"


def markdown(g):
    out = [f"# Book {g['book']}: {g['title']}", ""]
    if g["summary"]:
        out += [g["summary"].strip(), ""]
    s, f = g["score"], g["finish"]
    out += [
        "## The numbers",
        "",
        f"Played as well as possible, the {g['characters']} solved characters finish this book in {pct(f['median'])} of games (median), "
        f"from {pct(f['min'])} to {pct(f['max'])}. Their chance of winning the whole series from here, through Book 5, is "
        f"{pct(s['median'])} (median), from {pct(s['min'])} to {pct(s['max'])}. Points below are points of that chance.",
        "",
    ]
    out += ["## What to bring", "", "Mean chance of winning the series, for the characters that have each, and those that don't.", "",
            "| Discipline | With | Without | Difference |", "|---|---|---|---|"]
    out += [f"| {d['name']} | {pct(d['with_'])} | {pct(d['without'])} | {pts(d['gain'])} |" for d in g["disciplines"]]
    if g["items"]:
        out += ["", "| Starting item | With | Without | Difference |", "|---|---|---|---|"]
        out += [f"| {d['name']} | {pct(d['with_'])} | {pct(d['without'])} | {pts(d['gain'])} |" for d in g["items"]]
    if g["gold"]:
        label = "Gold Crowns of the pouch" if g["book"] == 1 else "Starting Gold Crowns"
        out += ["", f"| {label} | Mean chance |", "|---|---|"]
        out += [f"| {d['gold']} | {pct(d['mean'])} |" for d in g["gold"]]
    out += ["", "These compare groups of characters, so a difference also carries what the rest of the kit does with it.", ""]
    out += ["## The decisions that matter", "",
            "Ordered by what a wrong choice costs on average: the chance of reaching the decision, times what is lost there.", ""]
    for d in g["decisions"]:
        if d["kind"] == "section":
            where = "Section " + ", then ".join(str(s) for s in d["sections"])
            reach = f"reached in {pct(d['reached'])} of games"
        else:
            where = f"Fighting {d['opponent']}"
            reach = "a choice at each round"
        out += [f"### {where}", ""]
        if d["text"]:
            out += [f"> {d['text']}", ""]
        out += [f"Costs {pts(d['cost'])} points on average if chosen wrong, and up to {pts(d['worst'])}; {reach}, "
                f"a real choice for {d['characters']} of {g['characters']} characters.", ""]
        for b in d["best"]:
            out.append(f"- {b['choice']} ({pct(b['share'])} of characters)")
        for s in d["steps"]:
            out.append(f"- Then at section {s['section']}: " + "; ".join(f"{b['choice']} ({pct(b['share'])})" for b in s["best"]))
        # a flag is the solver's own name for something that happened earlier
        for r in d["rules"]:
            if not r["internal"]:
                out.append(f"- Rule: {r['rule']} ({r['characters']} characters)")
        out.append("")
    out += ["## Where characters die", "", "| Section | Opponent | Chance of dying there | Share of deaths | Opening |", "|---|---|---|---|---|"]
    out += [f"| {d['section']} | {d['opponent'] or ''} | {pct(d['share'])} | {pct(d['of_deaths'])} | {(d['text'] or '').replace('|', '/')} |"
            for d in g["deaths"]]
    out += ["", f"Generated by `report/guide.py` on {g['generated']} from commit {g['commit']}. The rules are patterns that fit at least 98% of "
            "a character's solved situations, see N4 in ASSUMPTIONS.md, and can stand for an earlier choice.", ""]
    return "\n".join(out)


def main():
    number = int(sys.argv[1])
    g = build(number)
    os.makedirs(os.path.join(P.ROOT, "guides"), exist_ok=True)
    with open(os.path.join(P.ROOT, f"guides/book{number:02d}.json"), "w") as f:
        json.dump(g, f, indent=1)
    with open(os.path.join(P.ROOT, f"guides/book{number:02d}.md"), "w") as f:
        f.write(markdown(g))


if __name__ == "__main__":
    main()
