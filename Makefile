all: check

%.p %.lst: %.asm
	asl -cpu 6502 -L $<

%.bin: %.p
	p2bin -r '$$-$$' $<

check:	a2bubble.bin
	echo '3742cf5494927a6cac01259ec8141e0beffc1ef6914e02acc15c8c77a53884f8  a2bubble.bin' | sha256sum -c

hexdiff:	a2bubble.bin
	hexdiff helix-a2-3.40.bin a2bubble.bin
