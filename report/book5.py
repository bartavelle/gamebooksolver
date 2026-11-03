#!/usr/bin/env python3
"""Writes the page for players of Book 5: what each kit is worth over all the solved cases, then how to play three of them.

usage: book5.py <output html>
"""
import glob
import html
import json
import os
import statistics
import sys

import analyse as A
import page as P

esc = html.escape
DATA = os.path.join(P.ROOT, "data/B05")
SKIPPED = {"TR": "Tracking", "HU": "Hunting", "6S": "Sixth Sense", "MO": "Mind Over Matter", "CA": "Camouflage", "MS": "Mindshield"}

CSS = """
.tool { display: grid; grid-template-columns: minmax(0, 1fr) minmax(0, 15rem); gap: 0; border: 1.5px solid var(--ink); background: var(--sheet); }
.fields { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 14px; padding: 16px; }
.fields label { display: flex; flex-direction: column; gap: 4px; font: 500 .68rem/1.3 var(--data); letter-spacing: .08em; text-transform: uppercase; color: var(--quiet); }
.fields select { font: 400 1rem/1.3 var(--body); color: var(--ink); background: var(--ground); border: 1px solid var(--line); padding: 7px 8px; width: 100%; }
.fields select:focus-visible, .tabs button:focus-visible { outline: 2px solid var(--lapis); outline-offset: 2px; }
.result { padding: 16px; border-left: 1px solid var(--line); display: flex; flex-direction: column; gap: 10px; }
.result b { font: 500 .68rem/1.3 var(--data); letter-spacing: .08em; text-transform: uppercase; color: var(--quiet); }
.result .big { font: 600 2.6rem/1 var(--display); font-variant-numeric: tabular-nums; color: var(--lapis); }
.result ul { padding: 0; list-style: none; gap: 4px; font-size: .9rem; }
.result li { display: flex; justify-content: space-between; gap: 10px; }
.result li span:last-child { font-family: var(--data); font-variant-numeric: tabular-nums; white-space: nowrap; }
.tabs { display: flex; flex-wrap: wrap; gap: 8px; }
.tabs button { font: 400 .95rem/1.2 var(--body); color: var(--ink); background: var(--sheet); border: 1px solid var(--line); padding: 8px 12px; cursor: pointer; display: flex; flex-direction: column; gap: 3px; text-align: left; }
.tabs button small { font: 500 .78rem/1 var(--data); color: var(--quiet); font-variant-numeric: tabular-nums; }
.tabs button[aria-selected="true"] { border-color: var(--lapis); box-shadow: inset 0 0 0 1px var(--lapis); }
.kit { display: flex; flex-direction: column; gap: 56px; }
.rules { display: flex; flex-direction: column; }
.rules > div { display: grid; grid-template-columns: 8rem minmax(0, 1fr); gap: 4px 20px; padding-block: 12px; border-top: 1px solid var(--line); }
.rules dt { font: 600 1.05rem/1.3 var(--display); color: var(--lapis); }
.rules dd { margin: 0; max-width: 62ch; }
.rules dl, dl.rules { margin: 0; }
@media (max-width: 640px) {
  .tool { grid-template-columns: minmax(0, 1fr); }
  .result { border-left: 0; border-top: 1px solid var(--line); }
  .rules > div { grid-template-columns: minmax(0, 1fr); }
}
"""

RULES = [
    ("Haakon", "When the stone falls at 335 or 204, grab it. Attacking him at 390 is certain death."),
    ("Potions", "Keep strength potions for the Dhorgaan at 253. With a potent potion, drink that one there. The Potion of Alether then sometimes goes on the Drakkar instead."),
    ("Potion of Alether", "Buy one for 4 Crowns in the shop at 154, through 54. This is why starting with one adds so little without the Sommerswerd."),
    ("Jewelled Mace", "It adds 5 Combat Skill against the Dhorgaan. You get it in the Yas's chest at 35, or from the vaxeler at 281 in exchange for the Oede Herb."),
    ("The Itikar", "Without the Sommerswerd, fly east to Chula at 343. With it, fly south. The Sommerswerd is what beats the Vordak on that road."),
    ("Mindshield", "It is the discipline to keep. Without it you lose Endurance before the Dhorgaan and fight the Yas at a penalty."),
]

SCRIPT = """
const SCORES = %s;
const FIELDS = ['skip', 'weapon', 'armor', 'head', 'alether', 'potent', 'elix'];
const LABEL = {
  weapon: {Sommerswerd: 'Sommerswerd'}, armor: {1: 'Chainmail Waistcoat'}, head: {H: 'Helmet', S: 'Silver Helm'},
  alether: {1: 'Potion of Alether'}, potent: {1: 'Potent Strength Potion'}, elix: {1: 'Having fought an Elix'},
  skip: {TR: 'Skip Tracking', HU: 'Skip Hunting', '6S': 'Skip Sixth Sense', MO: 'Skip Mind Over Matter', CA: 'Skip Camouflage', MS: 'Skip Mindshield'},
};
const read = () => Object.fromEntries(FIELDS.map(f => [f, document.getElementById('kit-' + f).value]));
const score = k => SCORES[FIELDS.map(f => k[f]).join('|')];
function update() {
  const k = read(), s = score(k), out = [];
  document.getElementById('kit-score').textContent = (s * 100).toFixed(1) + '%%';
  for (const f of FIELDS) {
    for (const o of document.getElementById('kit-' + f).options) {
      if (o.value === k[f] || !LABEL[f][o.value]) continue;
      const d = score({...k, [f]: o.value}) - s;
      if (d > 0.0005) out.push([LABEL[f][o.value], d]);
    }
  }
  out.sort((a, b) => b[1] - a[1]);
  const list = document.getElementById('kit-better');
  list.replaceChildren(...out.slice(0, 4).map(([n, d]) => {
    const li = document.createElement('li'), a = document.createElement('span'), b = document.createElement('span');
    a.textContent = n; b.textContent = '+' + (d * 100).toFixed(1);
    li.append(a, b); return li;
  }));
  document.getElementById('kit-better-title').hidden = out.length === 0;
}
FIELDS.forEach(f => document.getElementById('kit-' + f).addEventListener('change', update));
update();
const tabs = [...document.querySelectorAll('.tabs button')];
tabs.forEach(t => t.addEventListener('click', () => {
  tabs.forEach(o => {
    const on = o === t;
    o.setAttribute('aria-selected', on);
    document.getElementById(o.getAttribute('aria-controls')).hidden = !on;
  });
}));
"""


def parse(path):
    head, *rest = os.path.basename(path).rsplit(".", 1)[0].split("-")
    return dict(
        skip=head.split(".")[1],
        weapon="Sommerswerd" if "Sommerswerd" in rest else "Sword",
        armor="1" if "BodyArmor" in rest else "0",
        head="H" if "Helmet" in rest else "S" if "SilverHelmet" in rest else "-",
        alether="1" if "StrengthPotion" in rest else "0",
        potent="1" if "StrengthPotion4" in rest else "0",
        elix="1" if "FLGFoughtElix" in rest else "0",
    )


def key(k):
    return "|".join(k[f] for f in ("skip", "weapon", "armor", "head", "alether", "potent", "elix"))


def main():
    cases = {}
    paths = {}
    for path in glob.glob(os.path.join(DATA, "*.json")):
        k = parse(path)
        cases[key(k)] = json.load(open(path))["msentries"][0]["mscore"]
        paths[key(k)] = path[: -len(".json")]
    assert len(cases) == 576

    def diff(field, a, b):
        ds = []
        for path in paths.values():
            k = parse(path + ".json")
            if k[field] == a:
                ds.append(cases[key({**k, field: b})] - cases[key(k)])
        return statistics.mean(ds), max(ds)

    items = [
        ("Potent Strength Potion", *diff("potent", "0", "1")),
        ("Silver Helm, over no helmet", *diff("head", "-", "S")),
        ("Sommerswerd, over a Sword", *diff("weapon", "Sword", "Sommerswerd")),
        ("Chainmail Waistcoat", *diff("armor", "0", "1")),
        ("Helmet, over no helmet", *diff("head", "-", "H")),
        ("Potion of Alether", *diff("alether", "0", "1")),
        ("Having fought an Elix", *diff("elix", "0", "1")),
    ]
    top = max(m for _, m, _ in items)
    item_rows = "".join(
        f'<tr><td>{esc(n)}</td><td class="n">+{m * 100:.1f}</td><td class="n">+{x * 100:.1f}</td>'
        f'<td class="w"><div class="bar"><i style="width:{m / top * 100:.1f}%"></i></div></td></tr>'
        for n, m, x in items
    )
    by_skip = sorted(
        ((name, statistics.mean(v for kk, v in cases.items() if kk.split("|")[0] == s)) for s, name in SKIPPED.items()),
        key=lambda x: -x[1],
    )
    skip_rows = "".join(
        f'<tr><td>{esc(n)}</td><td class="n">{v * 100:.1f}%</td>'
        f'<td class="w"><div class="bar"><i style="width:{v * 100:.1f}%"></i></div></td></tr>'
        for n, v in by_skip
    )

    names = A.Names(json.load(open(os.path.join(P.ROOT, "report/items.json")))["5"])
    book = P.Book(5)
    plain = dict(skip="TR", armor="0", head="-", alether="0", potent="0", elix="0")
    kits = [
        ("sword", "Sword, nothing else", key({**plain, "weapon": "Sword"})),
        ("sommerswerd", "Sommerswerd, nothing else", key({**plain, "weapon": "Sommerswerd"})),
        ("strong", "Sommerswerd, Chainmail, Silver Helm, both potions",
         key(dict(skip="TR", weapon="Sommerswerd", armor="1", head="S", alether="1", potent="1", elix="0"))),
    ]
    tabs, panels = [], []
    for i, (slug, title, k) in enumerate(kits):
        res = P.kit_body(paths[k] + ".decisions.jsonl.zst", 5, book, names)
        on = i == 0
        tabs.append(
            f'<button type="button" id="tab-{slug}" role="tab" aria-controls="panel-{slug}" aria-selected="{"true" if on else "false"}">'
            f'{esc(title)}<small>{res["score"] * 100:.1f}%</small></button>'
        )
        panels.append(f'<div class="kit" id="panel-{slug}" role="tabpanel"{"" if on else " hidden"}>{res["body"]}</div>')

    def select(field, label, options, chosen):
        opts = "".join(f'<option value="{v}"{" selected" if v == chosen else ""}>{esc(t)}</option>' for v, t in options)
        return f'<label for="kit-{field}">{esc(label)}<select id="kit-{field}">{opts}</select></label>'

    yesno = [("0", "No"), ("1", "Yes")]
    fields = "".join([
        select("skip", "Discipline skipped", list(SKIPPED.items()), "TR"),
        select("weapon", "Main weapon", [("Sword", "Sword"), ("Sommerswerd", "Sommerswerd")], "Sommerswerd"),
        select("armor", "Chainmail Waistcoat", yesno, "0"),
        select("head", "Helmet", [("-", "None"), ("H", "Helmet"), ("S", "Silver Helm")], "-"),
        select("alether", "Potion of Alether", yesno, "0"),
        select("potent", "Potent Strength Potion", yesno, "0"),
        select("elix", "Fought an Elix before", yesno, "0"),
    ])
    rules = "".join(f"<div><dt>{esc(a)}</dt><dd>{esc(b)}</dd></div>" for a, b in RULES)
    scores = sorted(cases.values())
    page = f"""<title>Surviving Shadow on the Sand</title>
<meta name="claude:viewport-fit-sides" content="cover">
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=IBM+Plex+Sans:wght@400;600&family=Zilla+Slab:wght@600&display=swap">
<style>{P.CSS}{CSS}</style>
<div class="wrap">
<header>
  <div class="eyebrow">Lone Wolf, Book 5, an unofficial guide</div>
  <h1>Surviving Shadow on the Sand</h1>
  <p class="lead">A solver played every option of the book for {len(cases)} starting kits. The weakest character, with 20 Endurance and 10 Combat Skill, finishes between {scores[0] * 100:.0f}% and {scores[-1] * 100:.0f}% of the time. It can't do better than about 90%, because the fall from the Itikar at 261 is a flat 1 in 10, whatever you carry.</p>
</header>
<section>
  <h2>What is your kit worth?</h2>
  <p class="quiet">Pick what you start with. The list shows the changes that help most, in points. A point is one percentage point of the chance of finishing.</p>
  <div class="tool">
    <div class="fields">{fields}</div>
    <div class="result">
      <b>Chance of finishing</b>
      <div class="big" id="kit-score">{cases[key({**plain, "weapon": "Sommerswerd"})] * 100:.1f}%</div>
      <b id="kit-better-title">Would help most</b>
      <ul id="kit-better"></ul>
    </div>
  </div>
</section>
<section>
  <h2>What each item adds</h2>
  <p class="quiet">Comparing kits that differ only by this item, over all {len(cases)} kits.</p>
  <div class="scroll"><table><thead><tr><th>Item</th><th class="n">Average</th><th class="n">At most</th><th></th></tr></thead><tbody>{item_rows}</tbody></table></div>
</section>
<section>
  <h2>Which discipline to skip</h2>
  <p class="quiet">The average chance of finishing when this is the discipline you don't have. Healing, Animal Kinship, Mind Blast and Weapon Skill are in every kit, so this page says nothing about them.</p>
  <div class="scroll"><table><thead><tr><th>Skipped</th><th class="n">Finishes</th><th></th></tr></thead><tbody>{skip_rows}</tbody></table></div>
</section>
<section>
  <h2>True of nearly every kit</h2>
  <dl class="rules">{rules}</dl>
</section>
<section>
  <h2>Play one of these kits</h2>
  <p class="quiet">All three skip Tracking. Choose one to see its decisions, potions, deaths and fights.</p>
  <div class="tabs" role="tablist">{"".join(tabs)}</div>
</section>
{"".join(panels)}
<section>
  <h2>How to read this</h2>
  <ul>
    <li>All the kits start with 20 Endurance, 10 Combat Skill, 50 Crowns, two Meals, a Shield and a Potion of Laumspur, and Sword as the Weapon Skill. A character with a better roll may have other best choices.</li>
    <li>At 289 the advice to discard the Sommerswerd uses a choice added to the solver. The book only says to turn to 311 if you have it. There it gets destroyed anyway and costs 3 Endurance, so the gain is at most 4.6 points. Keeping it is the choice the book describes.</li>
    <li>When choices tie exactly, the first one in the book is shown, so some of the "reached" figures depend on that order.</li>
    <li>A test such as "when you have an item" fits at least 98% of the solved situations. It may only be a sign of an earlier choice, and not the reason for the advice.</li>
    <li>These figures assume the solver's reading of the rules is right. They are not an official Project Aon or Joe Dever publication.</li>
  </ul>
</section>
</div>
<script>{SCRIPT % json.dumps(cases, separators=(",", ":"))}</script>
"""
    with open(sys.argv[1], "w") as f:
        f.write(page)


if __name__ == "__main__":
    main()
