build: a.out microcode/hex

a.out: main.v
	iverilog main.v

microcode/hex: microcode/bin
	xxd -u -ps -c 4 microcode/bin microcode/hex

microcode/bin: microcode/microcode.tmcs
	python toolchain/mcasm.py microcode/microcode.tmcs microcode/bin

run: build microcode/hex
	vvp a.out

clean:
	rm a.out
	rm microcode/bin
	rm microcode/hex
