"""Turns the output of the `decisions` command into what a player needs to know.

Everything here only relies on the choices and scores found by the solver. The chances and scores must add up, or this
fails, see `check`.
"""
import collections
import json
import re

TIE = 1e-9


def load(path):
    states = {}
    root = None
    with open(path) as f:
        for line in f:
            st = json.loads(line)
            states[st["state"]] = st
            if root is None:
                root = st["state"]
    return root, states


def best_next(st):
    return st["choices"][st["best"]]["next"] if st["choices"] else []


def reach(root, states):
    """The chance of being in each state when always making the best choice."""
    pending = collections.Counter()
    for st in states.values():
        for _, nxt in best_next(st):
            pending[nxt] += 1
    chances = collections.Counter({root: 1.0})
    todo = [root]
    while todo:
        cur = todo.pop()
        for p, nxt in best_next(states[cur]):
            chances[nxt] += chances[cur] * p
            pending[nxt] -= 1
            if pending[nxt] == 0:
                todo.append(nxt)
    return chances


def check(root, states, chances):
    leaves = [s for s in states.values() if s["score"] is not None]
    total = sum(chances[s["state"]] for s in leaves)
    score = sum(chances[s["state"]] * s["score"] for s in leaves)
    expected = states[root]["choices"][states[root]["best"]]["score"]
    assert abs(total - 1) < 1e-9, f"the chances of the end states sum to {total}"
    assert abs(score - expected) < 1e-9, f"{score} instead of {expected}"
    return expected


class Names:
    def __init__(self, names):
        self.names = names

    def item(self, raw):
        m = re.fullmatch(r"Weapon\((\w+)\)", raw)
        if m:
            raw = m[1]
        m = re.fullmatch(r"GenSpecial\((\d+)\)", raw)
        if m:
            raw = "SGen" + m[1]
        m = re.fullmatch(r"GenBackpack\((\d+)\)", raw)
        if m:
            raw = "BGen" + m[1]
        return self.names.get(raw, re.sub(r"(?<=[a-z])(?=[A-Z])", " ", raw))


def effects(desc):
    """The items gained and lost according to the description of a choice, if it is only made of that."""
    if desc and not desc.startswith("[") and "GainItem" not in desc and "LoseItem" not in desc:
        return None
    return re.findall(r"(Gain|Lose)Item\((.+?), (\d+)\)", desc)


def clean_label(desc):
    desc = re.sub(r"\s*\[[^\]]*\]", "", desc)
    desc = re.sub(r"\s+", " ", desc).strip()
    m = re.search(r",? (?:and )?turn(?: immediately)? to (\d+)\.?$", desc)
    target = m[1] if m else None
    if m:
        desc = desc[: m.start()]
    desc = re.sub(r"^If you (?:wish|decide|want) (?:simply )?to ", "", desc).rstrip(".")
    desc = desc[0].upper() + desc[1:] if desc else desc
    return f"{desc} (turn to {target})" if target else desc


def describe(choice, other, names):
    """What this choice means for a player, next to the other one."""
    desc = choice["desc"]
    if "strength potion" in desc:
        return "Drink the Potion of Alether" if desc.startswith("Use") else "Save the Potion of Alether"
    mine, theirs = effects(desc), effects(other["desc"])
    if mine is None:
        return clean_label(desc)
    theirs = theirs or []
    gains = [names.item(i) for k, i, _ in mine if k == "Gain" and ("Gain", i, _) not in theirs]
    losses = [names.item(i) for k, i, _ in mine if k == "Lose" and ("Lose", i, _) not in theirs]
    missing = [names.item(i) for k, i, _ in theirs if k == "Gain" and ("Gain", i, _) not in mine]
    if gains and losses:
        return f"Drop {', '.join(losses)} for {', '.join(gains)}"
    if gains:
        return "Take " + ", ".join(gains)
    if losses:
        return "Drop " + ", ".join(losses)
    if missing:
        return "Leave " + ", ".join(missing)
    return "Take the same items in another order"


def features(st, names):
    """What can be read on the Action Chart, as (description of the test, value) pairs."""
    cv = st["cvar"]
    out = {}
    for n in range(1, 40):
        out[f"Endurance is {n} or less"] = cv["endurance"] <= n
    if st["opponent_endurance"] is not None:
        for n in range(1, 60):
            out[f"the opponent has {n} Endurance or less"] = st["opponent_endurance"] <= n
    held = {}
    for part in [p for p in cv["items"].split("/") if p]:
        name, _, q = part.partition(":")
        held[name] = int(q or 1)
    for n in range(1, 51):
        out[f"you have {n} or more Gold Crowns"] = held.get("Gold", 0) >= n
    for n in range(1, 8):
        out[f"you have {n} or more Meals"] = held.get("Meal", 0) >= n
    for name in ("Laumspur", "StrengthPotion", "Potion2Hp", "Potion4Hp", "Potion6Hp", "SGen1", "SGen3", "SGen4", "SGen5",
                 "SGen6", "SGen8", "BGen0", "BGen1", "BGen2", "Spear", "Dagger", "Sword", "Mace", "Warhammer",
                 "Quarterstaff", "BroadSword", "Shield", "Backpack", "Sommerswerd"):
        out[f"you have the {names.item(name)}"] = name in held
    for flag in cv["flags"]:
        if flag != "HadCombat":
            out[f"flag {flag}"] = True
    stored = {part.partition(":")[0] for part in cv["stored"].split("/") if part}
    for name in stored:
        out[f"your confiscated equipment includes the {names.item(name)}"] = True
    out["your equipment is confiscated"] = bool(stored)
    return out


def find_rule(rows, names):
    """A single test on the Action Chart that tells which of the two main picks is the best. rows: (state, chance, pick)"""
    total = sum(c for _, c, _ in rows)
    picks = collections.Counter()
    for _, c, p in rows:
        picks[p] += c
    if len(picks) < 2:
        return None
    (a, _), (b, _) = picks.most_common(2)
    tests = collections.defaultdict(lambda: [0.0, 0.0])
    feats = [(features(st, names), c, p) for st, c, p in rows]
    allnames = set()
    for f, _, _ in feats:
        allnames |= set(f)
    best = None
    for test in allnames:
        right = sum(c for f, c, p in feats if (p == b) == bool(f.get(test, False)))
        for share, pick_if, other in ((right / total, b, a), (1 - right / total, a, b)):
            if best is None or share > best[0] or (share == best[0] and len(test) < len(best[1])):
                best = (share, test, pick_if, other)
    # a rule that isn't nearly always right would only be misleading
    return best if best and best[0] >= 0.98 else None


def analyse(root, states, chances, names, display):
    groups = collections.defaultdict(list)
    ties = 0
    withchoice = 0
    for st in states.values():
        if len(st["choices"]) < 2:
            continue
        withchoice += 1
        chance = chances[st["state"]]
        best = st["choices"][st["best"]]
        others = sorted((c for i, c in enumerate(st["choices"]) if i != st["best"]), key=lambda c: -c["score"])
        lost = best["score"] - others[0]["score"]
        if lost < TIE:
            ties += 1
        key = ("fight", st["opponent"]) if st["opponent"] else ("chapter", st["chapter"])
        groups[key].append((st, chance, lost, best, others[0]))

    decisions = {}
    for key, rows in groups.items():
        cost = sum(c * l for _, c, l, _, _ in rows)
        chance = sum(c for _, c, _, _, _ in rows)
        worst = max(l for _, _, l, _, _ in rows)
        firm = [(st, c, describe(b, o, names)) for st, c, l, b, o in rows if l >= TIE]
        picks = collections.Counter()
        alts = collections.Counter()
        for st, c, l, b, o in rows:
            if l >= TIE:
                picks[describe(b, o, names)] += c
                alts[describe(o, b, names)] += c
        rule = find_rule(firm, names) if firm else None
        decisions[key] = dict(key=key, cost=cost, chance=chance, worst=worst, states=len(rows), picks=picks, alts=alts, rule=rule,
                              firm=sum(c for _, c, _ in firm))

    # consecutive decisions that are the same route are shown together
    def following(st):
        """The next decision after this state, on the most likely outcome of the best choice."""
        seen = set()
        cur = st
        while True:
            nxt = best_next(cur)
            if not nxt:
                return None
            _, name = max(nxt, key=lambda x: x[0])
            if name in seen or name not in states:
                return None
            seen.add(name)
            cur = states[name]
            if len(cur["choices"]) > 1:
                return cur

    parent = {}

    def find(k):
        while parent.get(k, k) != k:
            k = parent[k]
        return k

    for key, rows in groups.items():
        if key[0] != "chapter":
            continue
        st = max(rows, key=lambda r: r[1])[0]
        nxt = following(st)
        if nxt is None or nxt["opponent"]:
            continue
        other = ("chapter", nxt["chapter"])
        a, b = decisions[key], decisions.get(other)
        if b and other != key and abs(a["cost"] - b["cost"]) < 0.02 * max(a["cost"], b["cost"]) and a["cost"] > 0.002:
            parent[find(other)] = find(key)
    routes = collections.defaultdict(list)
    for key in decisions:
        routes[find(key)].append(decisions[key])
    return dict(decisions=decisions, routes=routes, ties=ties, withchoice=withchoice)


def deaths(states, chances):
    out = collections.Counter()
    for st in states.values():
        m = re.fullmatch(r"lost:(\d+)", st["state"])
        if m:
            out[int(m[1])] += chances[st["state"]]
    return out
