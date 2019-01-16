arch32Targets = bnw
arch64Targets = helloworld64 subr hw-inc hw-inc-proper asmprint cmd-args fizzbuzz cpuid helloworld hw-macro unaligned-stack unaligned-stack-2 echo-num

allBins = $(arch32Targets) $(arch64Targets)

arch32Bins = $(patsubst %,bin/%,$(arch32Targets))
arch64Bins = $(patsubst %,bin/%,$(arch64Targets))

all: $(allBins)

$(allBins): %: bin/%

# Link objects files

$(arch32Bins): bin/%: build32/%.o | bin
	ld -macosx_version_min 10.13.0 -lc -e _main $< -o $@

$(arch64Bins): bin/%: build64/%.o | bin
	ld -macosx_version_min 10.13.0 -lc -e _main $< -o $@

# Compile ASM to object files
build32/%.o: %.asm | build32
	nasm -f macho -g $< -o $@

build64/%.o: %.asm | build64
	nasm -f macho64 -g $< -o $@

# Create DIRs
build32:
	mkdir build32

build64:
	mkdir build64

bin:
	mkdir bin

### MISC

.PHONY: clean

clean:
	rm -rf build32/ build64/ bin/
