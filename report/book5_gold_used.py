"""How much starting gold is enough to replay each stored 50-gold solution of Book 5, see G5 in ASSUMPTIONS.md.

Run from the root of the repository. It reads the `.decisions.jsonl.zst` files of `data/B05` and prints a line for each,
then how many characters need each amount.

Going back from the end, a purchase adds its price to what must be held, a pickup takes off its amount, and losing all the
gold resets the need. Pickups come from the book, as a full pouch hides them in the solution. Paying at c248, c265 and c276
leads to the same place as not paying, so a replay does not pay. Gold that overflows when c14 gives it back is not a
purchase. The result is checked by replaying forward: that amount has to be enough, and one crown less must not be.
"""
import collections, glob, json, multiprocessing, os, re, subprocess, sys

sys.setrecursionlimit(1000000)
SHOPS = {27, 154, 169, 211, 248, 265, 276, 362, 388}
PAY_FOR_NOTHING = {248, 265, 276}
RETRIEVE = 14


def takes(o):
    if isinstance(o, list):
        return sum(takes(v) for v in o)
    if not isinstance(o, dict):
        return 0
    here = o["CanTake"][1] if "CanTake" in o and o["CanTake"][0] == "Gold" else 0
    return here + sum(takes(v) for v in o.values())


BOOK = {c[0]: c[1] for c in json.load(open("json-chapters/book05.json"))}
GAIN = {k: takes(v["pchoice"]) for k, v in BOOK.items() if takes(v["pchoice"])}


def one(path):
    states = {}
    for line in subprocess.run(["zstd", "-dc", path], capture_output=True, text=True, check=True).stdout.splitlines():
        d = json.loads(line)
        states[d["state"]] = d
    start = next(k for k, v in states.items() if v["chapter"] == 1)

    def total(k):
        v = states.get(k)
        if not v or not v.get("cvar"):
            return None
        return sum(int(x) for x in re.findall(r"Gold:(\d+)", v["cvar"]["items"] + " " + str(v["cvar"].get("stored", ""))))

    def nexts(k):
        v = states.get(k)
        if not v or not v.get("choices"):
            return []
        return [n for _, n in v["choices"][v["best"]]["next"]]

    def cost(k, n):
        """The gold paid going from k to n, None if all of it is lost."""
        a, b = total(k), total(n)
        ch = states[k]["chapter"]
        if b is None or b >= a or ch in PAY_FOR_NOTHING or ch == RETRIEVE:
            return 0
        if b == 0 and ch not in SHOPS:
            return None
        assert ch in SHOPS, (path, ch)
        return a - b

    def gain(k, n):
        ch = states[k]["chapter"]
        return GAIN.get(ch, 0) if n in states and states[n]["chapter"] != ch else 0

    memo = {}

    def need(k):
        if k not in memo:
            r = 0
            for n in nexts(k):
                c = cost(k, n)
                if c is not None:
                    r = max(r, c + max(0, need(n) - gain(k, n)))
            memo[k] = r
        return memo[k]

    def replay(gold):
        seen, stack = set(), [(start, gold)]
        while stack:
            k, g = stack.pop()
            if (k, g) in seen:
                continue
            seen.add((k, g))
            for n in nexts(k):
                c = cost(k, n)
                h = 0 if c is None else g - c
                if h < 0:
                    return False
                stack.append((n, min(50, h + gain(k, n))))
        return True

    x = need(start)
    return dict(name=os.path.basename(path).split(".decisions")[0], need=x, enough=replay(x), tight=x == 0 or not replay(x - 1))


if __name__ == "__main__":
    with multiprocessing.Pool(8) as pool:
        rows = pool.map(one, sorted(glob.glob("data/B05/*.decisions.jsonl.zst")))
    for r in rows:
        print(json.dumps(r))
    print(sorted(collections.Counter(r["need"] for r in rows).items()), "enough", sum(r["enough"] for r in rows), "tight", sum(r["tight"] for r in rows), "of", len(rows))
