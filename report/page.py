#!/usr/bin/env python3
"""Writes a page for players from the output of the `decisions` command of the explorer.

usage: page.py <decisions file> <book number> <title> <character json> <output html>
"""
import html
import json
import os
import re
import sys

import analyse as A

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..")
esc = html.escape

CSS = """
/* Layout: an Action Chart at the top, then numbered sections in the way a gamebook prints them, each with what is at stake. */
:root {
  --ground: #edf0f6; --sheet: #f8f9fc; --ink: #151c33; --quiet: #545d78; --line: #c6ccdc;
  --lapis: #23409c; --stake: #b0392b; --stake-soft: #efd5d0;
  --display: "Zilla Slab", Rockwell, Georgia, serif;
  --body: "IBM Plex Sans", "Helvetica Neue", Arial, sans-serif;
  --data: "IBM Plex Mono", ui-monospace, Menlo, monospace;
}
@media (prefers-color-scheme: dark) {
  :root:not([data-theme="light"]) {
    color-scheme: dark;
    --ground: #0e1322; --sheet: #161c30; --ink: #e7eaf4; --quiet: #a2abc4; --line: #303954;
    --lapis: #9db2f5; --stake: #ee8a79; --stake-soft: #47262a;
  }
}
:root[data-theme="dark"] {
  color-scheme: dark;
  --ground: #0e1322; --sheet: #161c30; --ink: #e7eaf4; --quiet: #a2abc4; --line: #303954;
  --lapis: #9db2f5; --stake: #ee8a79; --stake-soft: #47262a;
}
body { background: var(--ground); color: var(--ink); font: 400 16px/1.55 var(--body); }
.wrap { max-width: 860px; margin: 0 auto; padding-block: 48px 72px;
  padding-left: max(16px, env(safe-area-inset-left, 0px)); padding-right: max(16px, env(safe-area-inset-right, 0px));
  display: flex; flex-direction: column; gap: 56px; }
h1, h2 { font-family: var(--display); font-weight: 600; line-height: 1.1; margin: 0; text-wrap: balance; }
h1 { font-size: clamp(2.3rem, 7vw, 3.6rem); }
h2 { font-size: 1.65rem; }
p { margin: 0; max-width: 65ch; }
.eyebrow { font: 500 .75rem/1.3 var(--data); letter-spacing: .08em; text-transform: uppercase; color: var(--lapis); }
.quiet { color: var(--quiet); }
header, section { display: flex; flex-direction: column; gap: 16px; }
.lead { font-size: 1.15rem; }
.chart { display: grid; grid-template-columns: repeat(auto-fit, minmax(150px, 1fr)); border: 1.5px solid var(--ink); background: var(--sheet); }
.box { padding: 10px 14px; border-right: 1px solid var(--line); border-bottom: 1px solid var(--line); display: flex; flex-direction: column; gap: 4px; }
.box.wide { grid-column: 1 / -1; }
.box b { font: 500 .68rem/1.3 var(--data); letter-spacing: .08em; text-transform: uppercase; color: var(--quiet); }
.box span { font-family: var(--display); font-size: 1.05rem; line-height: 1.3; }
.box .big { font-size: 2rem; line-height: 1; font-variant-numeric: tabular-nums; }
.entries { display: flex; flex-direction: column; }
.entry { display: grid; grid-template-columns: 5.5rem minmax(0, 1fr) 11rem; gap: 8px 20px; padding-block: 18px; border-top: 1px solid var(--line); }
.num { font: 600 2rem/1 var(--display); color: var(--lapis); font-variant-numeric: tabular-nums; }
.num small { display: block; font: 400 .72rem/1.4 var(--data); color: var(--quiet); margin-top: 4px; }
.what { display: flex; flex-direction: column; gap: 8px; min-width: 0; }
.do { font-weight: 600; }
.step { display: flex; flex-direction: column; gap: 2px; }
.not { color: var(--quiet); font-size: .93rem; }
.rule { font-size: .93rem; }
.rule em { font-style: normal; font-weight: 600; }
.stake { display: flex; flex-direction: column; gap: 6px; font: 400 .78rem/1.35 var(--data); color: var(--quiet); }
.stake strong { font: 600 1.05rem/1 var(--data); color: var(--stake); font-variant-numeric: tabular-nums; }
.bar { height: 7px; background: var(--stake-soft); }
.bar i { display: block; height: 100%; background: var(--stake); }
.scroll { overflow-x: auto; }
table { border-collapse: collapse; width: 100%; font-size: .95rem; }
th { font: 500 .68rem/1.3 var(--data); letter-spacing: .08em; text-transform: uppercase; color: var(--quiet); text-align: left; padding: 8px 12px 8px 0; border-bottom: 1.5px solid var(--ink); }
td { padding: 10px 12px 10px 0; border-bottom: 1px solid var(--line); vertical-align: top; }
td.n, th.n { text-align: right; font-family: var(--data); font-variant-numeric: tabular-nums; white-space: nowrap; }
td.s { font: 600 1.15rem/1.2 var(--display); color: var(--lapis); font-variant-numeric: tabular-nums; white-space: nowrap; }
td.w { width: 26%; min-width: 90px; }
ul { margin: 0; padding-left: 1.2rem; display: flex; flex-direction: column; gap: 8px; max-width: 65ch; }
@media (max-width: 640px) {
  .wrap { padding-block: 28px 48px; gap: 40px; }
  .entry { grid-template-columns: 4rem minmax(0, 1fr); }
  .stake { grid-column: 2; }
}
"""


class Book:
    def __init__(self, number):
        self.chapters = dict(json.load(open(os.path.join(ROOT, f"json-chapters/book{number:02d}.json"))))

    def section(self, cid):
        """The section of the printed book: the highest ones are only steps added when transcribing it."""
        ch = self.chapters.get(cid)
        if ch is None or cid <= 400:
            return cid
        if str(ch["title"]).isdigit() and int(ch["title"]) != cid:
            return int(ch["title"])
        for other, d in self.chapters.items():
            if other <= 400 and re.search(rf'"Goto": {cid}\b', json.dumps(d["pchoice"])):
                return other
        return cid

    def opponent(self, cid):
        ch = self.chapters.get(cid)
        if ch is None:
            return None
        m = re.search(r'"opponent": "([^"]+)"', json.dumps(ch["pchoice"]))
        return m[1] if m else None

    def what_ends(self, cid):
        op = self.opponent(cid)
        if op:
            return f"Losing the fight with {op}"
        desc = re.sub(r"\s+", " ", self.chapters[cid]["desc"]).strip()
        return re.split(r"(?<=[.!?]) ", desc)[0]


def pct(x, digits=1):
    return f"{x * 100:.{digits}f}%"


def pts(x):
    return f"{x * 100:.1f}"


def stake(d):
    return (
        '<div class="stake">'
        f"<strong>−{pts(d['worst'])} pts</strong>"
        f'<div class="bar"><i style="width:{min(d["worst"], 1) * 100:.1f}%"></i></div>'
        f"<span>reached in {pct(d['chance'], 0)} of games</span></div>"
    )


def step(d, names):
    picks = d["picks"].most_common()
    out = []
    if d["rule"]:
        share, test, pick, other = d["rule"]
        out.append(f'<div class="do">{esc(other)}.</div>')
        out.append(f'<div class="rule">But <em>{esc(pick[0].lower() + pick[1:])}</em> when {esc(test)}.</div>')
    else:
        firm = d["firm"] or 1
        if len(picks) == 1:
            out.append(f'<div class="do">{esc(picks[0][0])}.</div>')
        else:
            out.append(f'<div class="do">{esc(picks[0][0])}.</div>')
            for name, c in picks[1:2]:
                out.append(f'<div class="rule">In {c / firm:.0%} of these games: <em>{esc(name)}</em>. No simple test explains it.</div>')
    if d["alts"] and not d["rule"]:
        out.append(f'<div class="not">Instead of: {esc(d["alts"].most_common(1)[0][0])}.</div>')
    return '<div class="step">' + "".join(out) + "</div>"


def is_potion(d):
    return any(re.fullmatch(r"Drink the .*otion.*|Save the potions", p) for p in d["picks"])


def kit_body(path, number, book, names):
    """What a player needs for one character: its chance of finishing, and the sections of the page."""
    root, states = A.load(path)
    chances = A.reach(root, states)
    score = A.check(root, states, chances)
    res = A.analyse(root, states, chances, names, None)
    deaths = A.deaths(states, chances)
    assert abs(sum(deaths.values()) - (1 - score)) < 1e-9

    routes = []
    potions = []
    for key, ds in res["routes"].items():
        if key[0] == "fight":
            continue
        if len(ds) == 1 and is_potion(ds[0]):
            potions.append(ds[0])
            continue
        ds = sorted(ds, key=lambda d: -d["chance"])
        routes.append(ds)
    routes.sort(key=lambda ds: -max(d["cost"] for d in ds))
    shown = [ds for ds in routes if max(d["cost"] for d in ds) >= 0.01]

    entries = []
    for ds in shown:
        lead = max(ds, key=lambda d: d["cost"])
        secs = [book.section(d["key"][1]) for d in ds]
        label = secs[0]
        more = f"<small>then {', '.join(str(s) for s in secs[1:])}</small>" if len(secs) > 1 else ""
        body = "".join(step(d, names) for d in ds)
        entries.append(f'<article class="entry"><div class="num">{label}{more}</div><div class="what">{body}</div>{stake(lead)}</article>')

    potions.sort(key=lambda d: -d["cost"])
    potion_rows = []
    for d in potions:
        cid = d["key"][1]
        pick = d["picks"].most_common(1)[0][0]
        potion_rows.append(
            f'<tr><td class="s">{book.section(cid)}</td><td>{esc(book.opponent(cid) or "?")}</td><td>{esc(pick)}</td>'
            f'<td class="n">{pct(d["chance"], 0)}</td><td class="n">−{pts(d["worst"])}</td></tr>'
        )

    death_rows = []
    biggest = max(deaths.values())
    for cid, c in deaths.most_common():
        if c < 0.0005:
            continue
        death_rows.append(
            f'<tr><td class="s">{book.section(cid)}</td><td>{esc(book.what_ends(cid))}</td><td class="n">{pct(c)}</td>'
            f'<td class="w"><div class="bar"><i style="width:{c / biggest * 100:.1f}%"></i></div></td></tr>'
        )
    small_deaths = sum(c for c in deaths.values() if c < 0.0005)

    fights = []
    for key, d in res["decisions"].items():
        if key[0] != "fight":
            continue
        rows = [st for st in states.values() if st["opponent"] == key[1] and len(st["choices"]) > 1]
        top = max(st["opponent_endurance"] for st in rows)
        entered = sum(chances[st["state"]] for st in rows if st["opponent_endurance"] == top)
        pick = d["picks"].most_common(1)[0][0] if d["picks"] else "Either"
        rule = f"{d['rule'][2]} when {d['rule'][1]}, otherwise {d['rule'][3].lower()}" if d["rule"] else f"{pick} as soon as you can" if pick == "Evade" else pick
        fights.append(f'<tr><td>{esc(key[1])}</td><td>{esc(rule)}</td><td class="n">{pct(entered, 0)}</td><td class="n">−{pts(d["worst"])}</td></tr>')

    body = f"""<section>
  <h2>The decisions that decide the book</h2>
  <p class="quiet">Numbers are the book's section numbers, ordered by how much a wrong choice costs on average. A point is one percentage point of the chance of finishing. The red figure is the most you can lose by taking the next best option.</p>
  <div class="entries">{"".join(entries)}</div>
</section>
<section>
  <h2>When to drink a strength potion</h2>
  <p class="quiet">The Potion of Alether adds 2 Combat Skill for one fight, and a potent one adds 4. These are the fights where the choice to drink matters, with the most a wrong call costs.</p>
  <div class="scroll"><table><thead><tr><th>Section</th><th>Fight</th><th>Potion</th><th class="n">Reached</th><th class="n">Points</th></tr></thead><tbody>{"".join(potion_rows)}</tbody></table></div>
</section>
<section>
  <h2>Where the remaining {pct(1 - score)} of games end</h2>
  <p class="quiet">These are the risks left after every choice is made well.</p>
  <div class="scroll"><table><thead><tr><th>Section</th><th>What happens</th><th class="n">Games</th><th></th></tr></thead><tbody>{"".join(death_rows)}</tbody></table></div>
  {f'<p class="quiet">Other sections add up to {pct(small_deaths, 2)}.</p>' if small_deaths else ""}
</section>
<section>
  <h2>Fights you can leave</h2>
  <p class="quiet">Only fights where the choice to evade changes the score are listed, with the most a wrong call costs.</p>
  <div class="scroll"><table><thead><tr><th>Opponent</th><th>What to do</th><th class="n">Reached</th><th class="n">Points</th></tr></thead><tbody>{"".join(fights)}</tbody></table></div>
</section>
"""
    return dict(score=score, body=body, withchoice=res["withchoice"], ties=res["ties"])


def main():
    path, number, title, char_path, out = sys.argv[1:6]
    number = int(number)
    char = json.load(open(char_path))
    names = A.Names(json.load(open(os.path.join(ROOT, "report/items.json")))[str(number)])
    kit = kit_body(path, number, Book(number), names)
    score = kit["score"]
    boxes = "".join(
        f'<div class="box{" wide" if len(v) > 40 else ""}"><b>{esc(k)}</b><span>{esc(v)}</span></div>' for k, v in char["chart"]
    )
    page = f"""<title>{esc(title)}</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;600&family=Zilla+Slab:wght@600&display=swap">
<style>{CSS}</style>
<div class="wrap">
<header>
  <div class="eyebrow">Lone Wolf, Book {number}</div>
  <h1>{esc(char["book"])}</h1>
  <p class="lead">Played as well as possible, this character finishes the book in {pct(score)} of games. Most of the choices along the way barely matter. These are the ones that do.</p>
  <div class="chart">
    <div class="box"><b>Chance of finishing</b><span class="big">{pct(score)}</span></div>
    {boxes}
  </div>
</header>
{kit["body"]}<section>
  <h2>How to read this</h2>
  <ul>
    {"".join(f"<li>{esc(x)}</li>" for x in char["notes"])}
    <li>The solver found {kit["withchoice"]} situations with a choice on the best path, and {kit["ties"]} of them are exact ties.</li>
    <li>A test such as "when you have an item" is a pattern that fits at least 98% of the solved situations. It may only be a sign of an earlier choice, and not the reason for the advice.</li>
  </ul>
</section>
</div>
"""
    with open(out, "w") as f:
        f.write(page)


if __name__ == "__main__":
    main()
