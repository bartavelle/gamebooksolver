
DISCS = CA HU 6S TR HL SP SS MS MB AK MO CA.HU CA.TR CA.HL CA.SP CA.SS CA.MS CA.MB CA.MO HU.TR HU.SP HU.SS HU.MS HU.MB HU.MO 6S.CA 6S.HU 6S.TR 6S.HL 6S.SP 6S.SS 6S.MS 6S.MB 6S.AK 6S.MO HL.HU HL.TR HL.SP HL.SS HL.MS HL.MB HL.MO SP.TR SP.SS SS.TR MS.TR MS.SP MS.SS MB.TR MB.SP MB.SS MB.MS MB.MO AK.CA AK.HU AK.TR AK.HL AK.SP AK.SS AK.MS AK.MB AK.MO MO.TR MO.SP MO.SS MO.MS
# TARGETS = data/CA.cbor data/HU.cbor data/6S.cbor data/TR.cbor data/HL.cbor data/SP.cbor data/SS.cbor data/MS.cbor data/MB.cbor data/AK.cbor data/MO.cbor data/CA.HU.cbor data/CA.TR.cbor data/CA.HL.cbor data/CA.SP.cbor data/CA.SS.cbor data/CA.MS.cbor data/CA.MB.cbor data/CA.MO.cbor data/HU.TR.cbor data/HU.SP.cbor data/HU.SS.cbor data/HU.MS.cbor data/HU.MB.cbor data/HU.MO.cbor data/6S.CA.cbor data/6S.HU.cbor data/6S.TR.cbor data/6S.HL.cbor data/6S.SP.cbor data/6S.SS.cbor data/6S.MS.cbor data/6S.MB.cbor data/6S.AK.cbor data/6S.MO.cbor data/HL.HU.cbor data/HL.TR.cbor data/HL.SP.cbor data/HL.SS.cbor data/HL.MS.cbor data/HL.MB.cbor data/HL.MO.cbor data/SP.TR.cbor data/SP.SS.cbor data/SS.TR.cbor data/MS.TR.cbor data/MS.SP.cbor data/MS.SS.cbor data/MB.TR.cbor data/MB.SP.cbor data/MB.SS.cbor data/MB.MS.cbor data/MB.MO.cbor data/AK.CA.cbor data/AK.HU.cbor data/AK.TR.cbor data/AK.HL.cbor data/AK.SP.cbor data/AK.SS.cbor data/AK.MS.cbor data/AK.MB.cbor data/AK.MO.cbor data/MO.TR.cbor data/MO.SP.cbor data/MO.SS.cbor data/MO.MS.cbor
TARGETSHIGH = $(patsubst %, data/2919%.cbor, $(DISCS))
TARGETSMEDIUM = $(patsubst %, data/2515%.cbor, $(DISCS))
TARGETSLOW = $(patsubst %, data/2010%.cbor, $(DISCS))

.PHONY: all low medium high

all: low medium high
	echo ok

low: $(TARGETSLOW)
	echo "low done"

medium: $(TARGETSMEDIUM)
	echo "medium done"

high: $(TARGETSHIGH)
	echo "high done"

data/%.cbor: dist/gamebooksolver-solvebook02 buildsol.py
	python3 buildsol.py $@