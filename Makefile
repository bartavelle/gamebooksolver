SDISCS = CA HU 6S TR HL MS MB AK MO
DISCS2 = CA.HU CA.TR CA.HL CA.SP CA.MS CA.MB CA.MO HU.TR HU.SP HU.MS HU.MB HU.MO 6S.CA 6S.HU 6S.TR 6S.HL 6S.SP 6S.MS 6S.MB 6S.AK 6S.MO HL.HU HL.TR HL.SP HL.MS HL.MB HL.MO SP.TR MS.TR MS.SP MB.TR MB.SP MB.MS MB.MO AK.CA AK.HU AK.TR AK.HL AK.SP AK.MS AK.MB AK.MO MO.TR MO.SP MO.MS

TARGETSLOWB03simple = $(patsubst %, data/B03/2010SW.%.cbor, $(DISCS2))
TARGETSLOWB03 = $(TARGETSLOWB03simple)

TARGETSLOWB04simple = $(patsubst %, data/B04/2010SW.%.cbor, $(SDISCS))
TARGETSLOWB04sw = $(patsubst %, data/B04/2010SW.%-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(SDISCS))
TARGETSLOWB04helm = $(patsubst %, data/B04/2010SW.%-Sommerswerd-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(SDISCS))
TARGETSLOWB04helmsw = $(patsubst %, data/B04/2010SW.%-Sword-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(SDISCS))
TARGETSLOWB04 = $(TARGETSLOWB04simple) $(TARGETSLOWB04sw) $(TARGETSLOWB04helm) $(TARGETSLOWB04helmsw)

TARGETSLOWB05 = data/B05/2010SW.cbor data/B05/2010SW-FLGFoughtElix.cbor data/B05/2010SW-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor

ALLCOMBS = 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2110 2111 2112 2113 2114 2115 2116 2117 2118 2119 2210 2211 2212 2213 2214 2215 2216 2217 2218 2219 2310 2311 2312 2313 2314 2315 2316 2317 2318 2319 2410 2411 2412 2413 2414 2415 2416 2417 2418 2419 2510 2511 2512 2513 2514 2515 2516 2517 2518 2519 2610 2611 2612 2613 2614 2615 2616 2617 2618 2619 2710 2711 2712 2713 2714 2715 2716 2717 2718 2719 2810 2811 2812 2813 2814 2815 2816 2817 2818 2819 2910 2911 2912 2913 2914 2915 2916 2917 2918 2919

.PHONY: all low01 medium01 high01 book01 low05 book05 low05c

all: book05 book04
	echo ok

data/B01:
	mkdir data/B01

data/B02:
	mkdir data/B02

data/B03:
	mkdir data/B03

data/B04:
	mkdir data/B04

data/B05:
	mkdir data/B05

book05: low05c low05j low05d low05

low05c: data/B05 $(TARGETSLOWB05)
low05j: data/B05 $(patsubst %, %.jot, $(TARGETSLOWB05))
low05d: data/B05 $(patsubst %, %.dot, $(TARGETSLOWB05))
low05: data/B05 $(patsubst %, %.svg, $(TARGETSLOWB05))

book04: low04c low04j low04d low04

low04c: low05c data/B04 $(TARGETSLOWB04) data/B04/2010SW.6S-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal-FireSphereB04.cbor data/B04/2010SW.6S-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal-ropeB04.cbor data/B04/2010SW.6Sg10.cbor data/B04/2010SW.6Sg50.cbor
low04j: data/B04 $(patsubst %, %.jot, $(TARGETSLOWB04))
low04d: data/B04 $(patsubst %, %.dot, $(TARGETSLOWB04))
low04: data/B04 $(patsubst %, %.svg, $(TARGETSLOWB04))

low03c: low04c data/B03 $(TARGETSLOWB03)
low03j: data/B03 $(patsubst %, %.jot, $(TARGETSLOWB03))
low03d: data/B03 $(patsubst %, %.dot, $(TARGETSLOWB03))
low03: data/B03 $(patsubst %, %.svg, $(TARGETSLOWB03))


data/%.cbor:
	time python3 buildsol.py $@

data/%.cbor.jot: data/%.cbor gamebooksolver-explorer
	time ./gamebooksolver-explorer/target/release/gamebooksolver-explorer --solpath $< > $@

data/%.dot: data/%.jot
	./dist/gamebooksolver-solvebook dot $< > $@

data/%.svg: data/%.dot
	dot -Tsvg -o$@ $<
