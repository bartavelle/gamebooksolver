from typing import List

TARGETSLOWB04TP = [
    "",
    "-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal",
    "-Sommerswerd-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal",
    "-Sword-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal",
]


TARGETSLOWB05TP = [
    "",
    "-FLGFoughtElix",
    "-Sword-Meal-Meal-Shield-Laumspur",
    "-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix",
    "-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm",
    "-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm",
    "-Sword-Meal-Meal-Shield-Laumspur-SilverHelm",
    "-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm",
]

DISCS = ["CA", "HU", "6S", "TR", "HL", "MS", "MB", "AK", "MO"]

TGTS4: List[str] = []
for t in TARGETSLOWB05TP:
    for d1 in DISCS:
        for d2 in DISCS:
            if d1 > d2:
                TGTS4.append("data/B05/2010SW.%s.%s%s.cbor" % (d1, d2, t))

TGTS5: List[str] = []
for t in TARGETSLOWB05TP:
    for d in DISCS:
        TGTS5.append("data/B05/2010SW.%s%s.cbor" % (d, t))

print("TARGETSLOWB04 = " + " ".join(TGTS4))
print("TARGETSLOWB05 = " + " ".join(TGTS5))
