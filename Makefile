DISCS1 = CA HU 6S TR HL MS MB AK MO
DISCS2 = CA.HU CA.TR CA.HL CA.SP CA.MS CA.MB CA.MO HU.TR HU.SP HU.MS HU.MB HU.MO 6S.CA 6S.HU 6S.TR 6S.HL 6S.SP 6S.MS 6S.MB 6S.AK 6S.MO HL.HU HL.TR HL.SP HL.MS HL.MB HL.MO SP.TR MS.TR MS.SP MB.TR MB.SP MB.MS MB.MO AK.CA AK.HU AK.TR AK.HL AK.SP AK.MS AK.MB AK.MO MO.TR MO.SP MO.MS

TARGETSLOWB03simple = $(patsubst %, data/B03/2010SW.%.cbor, $(DISCS2))
TARGETSLOWB03 = $(TARGETSLOWB03simple)

TARGETSLOWB04simple = $(patsubst %, data/B04/2010SW.%.cbor, $(DISCS2))
TARGETSLOWB04sw = $(patsubst %, data/B04/2010SW.%-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(DISCS2))
TARGETSLOWB04helm = $(patsubst %, data/B04/2010SW.%-Sommerswerd-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(DISCS2))
TARGETSLOWB04helmsw = $(patsubst %, data/B04/2010SW.%-Sword-SilverHelm-Laumspur-Shield-Meal-Meal-Meal-Meal.cbor, $(DISCS2))
TARGETSLOWB04 = $(TARGETSLOWB04simple) $(TARGETSLOWB04sw) $(TARGETSLOWB04helm) $(TARGETSLOWB04helmsw)

TARGETSLOWB05 = data/B05/2010SW.CA.cbor data/B05/2010SW.HU.cbor data/B05/2010SW.6S.cbor data/B05/2010SW.TR.cbor data/B05/2010SW.HL.cbor data/B05/2010SW.MS.cbor data/B05/2010SW.MB.cbor data/B05/2010SW.AK.cbor data/B05/2010SW.MO.cbor data/B05/2010SW.CA-FLGFoughtElix.cbor data/B05/2010SW.HU-FLGFoughtElix.cbor data/B05/2010SW.6S-FLGFoughtElix.cbor data/B05/2010SW.TR-FLGFoughtElix.cbor data/B05/2010SW.HL-FLGFoughtElix.cbor data/B05/2010SW.MS-FLGFoughtElix.cbor data/B05/2010SW.MB-FLGFoughtElix.cbor data/B05/2010SW.AK-FLGFoughtElix.cbor data/B05/2010SW.MO-FLGFoughtElix.cbor data/B05/2010SW.CA-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.HU-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.6S-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.TR-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.HL-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.MS-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.MB-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.AK-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.MO-Sword-Meal-Meal-Shield-Laumspur.cbor data/B05/2010SW.CA-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.HU-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.6S-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.TR-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.HL-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.MS-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.MB-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.AK-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.MO-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix.cbor data/B05/2010SW.CA-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.HU-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.6S-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.TR-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.HL-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MS-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MB-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.AK-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MO-Sommerswerd-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.CA-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.HU-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.6S-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.TR-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.HL-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MS-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MB-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.AK-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MO-Sommerswerd-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.CA-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.HU-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.6S-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.TR-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.HL-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MS-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MB-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.AK-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.MO-Sword-Meal-Meal-Shield-Laumspur-SilverHelm.cbor data/B05/2010SW.CA-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.HU-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.6S-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.TR-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.HL-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MS-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MB-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.AK-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor data/B05/2010SW.MO-Sword-Meal-Meal-Shield-Laumspur-FLGFoughtElix-SilverHelm.cbor

.PHONY: all low01 medium01 high01 book01 low05 book05 low04 book04 

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

# data/B04/2010SW.6S-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal-FireSphereB04.cbor
# data/B04/2010SW.6S-Sword-Laumspur-Shield-Meal-Meal-Meal-Meal-ropeB04.cbor
# data/B04/2010SW.6Sg10.cbor
# data/B04/2010SW.6Sg50.cbor
low04c: low05j data/B04 $(TARGETSLOWB04)
low04j: data/B04 $(patsubst %, %.jot, $(TARGETSLOWB04))
low04d: data/B04 $(patsubst %, %.dot, $(TARGETSLOWB04))
low04: data/B04 $(patsubst %, %.svg, $(TARGETSLOWB04))

low03c: low04j data/B03 $(TARGETSLOWB03)
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
