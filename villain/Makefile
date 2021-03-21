UNAME := $(shell uname)
.PHONY: test std default
.PRECIOUS: libraries-lmdefs.s

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

objs = \
	main.o \
	char.o \
	io.o \
	symbol.o \
	str.o \
	wrap.o \
	utf8.o

# $(patsubst %.rkt,%.o,$(wildcard lib/*rkt)) \

default: runtime.o

main.o: villain.h runtime.h
wrap.o: villain.h types.h
char.o: villain.h utf8.h char.h
io.o: runtime.h villain.h utf8.h
symbol.o: str.h types.h villain.h
str.o: types.h villain.h

%.run:  %.o
	@ racket -t formdps.rkt -m make $@

%.run2: %.o runtime.o 
	gcc runtime.o $(shell cat modulefiles) $< -o $@ $(libs) # -lm
	@ racket -t formdps.rkt -m mv $@
	rm -f formdps $(shell cat modulefiles) modulefiles
	find . -name "*.s" -not -name libraries-lmdefs.s -delete

runtime.o: $(objs) libraries-lmdefs.o
	ld -r $(objs) libraries-lmdefs.o -o runtime.o

libraries-lmdefs.o: libraries-lmdefs.s
	nasm -f $(format) -o $@ $<

libraries-lmdefs.s: libraries-letrec

.SUFFIXES: .c .s .o .rkt

.c.o:
	gcc -fPIC -c -o $@ $<

.s.o:
	nasm -f $(format) -o $@ $<

lib/%.s: lib/%.rkt
	racket -t compile-library.rkt -m $< > $@

%.s: %.rkt libraries-letrec 
	racket -t compile-file.rkt -m $< > $@

libraries-letrec:
	racket compile-lib-letrec-lmdefs.rkt

clean:
	find .  -name "*.o" -not -name \
		"libraries-lmdefs.o" \
		-not -name "runtime.o" \
		-not -name "main.o" \
		-not -name "char.o" \
		-not -name "io.o" \
		-not -name "symbol.o" \
		-not -name "str.o" \
		-not -name "wrap.o" \
		-not -name "utf8.o"  -delete \
		-or -name "*.s" -not -name \
		"libraries-lmdefs.s" -delete \
		-or -name "*.run" -delete
	rm -f modulefiles formdps modules-example-*.*

rtcl:
	rm -f runtime.o $(objs)

libcl:
	rm -f libraries-letrec lib-externs lib-ls-ids \
	lib-fs libraries-lmdefs.s test/libraries-letrec \
	test/lib-fs test/lib-ls-ids test/lib-externs \
	libraries-lmdefs.o libraries-lmdefs

allcl:  clean libcl rtcl




