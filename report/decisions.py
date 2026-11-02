#!/usr/bin/env python3
"""Finds the decisions that matter in the output of the `decisions` command of the explorer.

A decision matters if it is likely to happen, and if the score is a lot lower with another choice.
usage: decisions.py <file> [number of decisions]
"""
import collections
import json
import re
import sys


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


def label(desc):
    """What the choice is, without the effects that are listed after it."""
    return re.sub(r"\s+", " ", desc).strip()[:110] or "(no text)"


def where(st):
    if st["opponent"]:
        return f"{st['opponent']} (opponent at {st['opponent_endurance']})"
    return f"chapter {st['chapter']}"


def main():
    root, states = load(sys.argv[1])
    top = int(sys.argv[2]) if len(sys.argv) > 2 else 25
    chances = reach(root, states)
    score = check(root, states, chances)

    decisions = collections.defaultdict(lambda: {"cost": 0.0, "chance": 0.0, "lost": 0.0, "states": 0, "picks": collections.Counter(), "alts": collections.Counter()})
    ties = 0
    total = 0
    for st in states.values():
        if len(st["choices"]) < 2:
            continue
        total += 1
        chance = chances[st["state"]]
        best = st["choices"][st["best"]]
        others = sorted((c for i, c in enumerate(st["choices"]) if i != st["best"]), key=lambda c: -c["score"])
        lost = best["score"] - others[0]["score"]
        if lost < 1e-12:
            ties += 1
        d = decisions[where(st)]
        d["cost"] += chance * lost
        d["chance"] += chance
        d["lost"] = max(d["lost"], lost)
        d["states"] += 1
        d["picks"][label(best["desc"])] += chance
        d["alts"][label(others[0]["desc"])] += chance

    print(f"score {score:.6f}, {len(states)} states on the best path, {total} with a choice, {ties} of them ties")
    print(f"{'cost':>8} {'reach':>6} {'worst':>6}  where")
    ranked = sorted(decisions.items(), key=lambda kv: -kv[1]["cost"])
    for name, d in ranked[:top]:
        print(f"{d['cost']:8.5f} {d['chance']:6.3f} {d['lost']:6.3f}  {name} [{d['states']} states]")
        for pick, c in d["picks"].most_common(2):
            print(f"{'':24}best: {pick} ({c / d['chance']:.0%})")
        print(f"{'':24}next: {d['alts'].most_common(1)[0][0]}")


if __name__ == "__main__":
    main()
