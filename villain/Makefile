UNAME := $(shell uname)
.PHONY: test std default

libs=-lunistring -lSDL2 -lm -pthread
ifeq ($(UNAME), Darwin)
  format=macho64
else
  format=elf64
endif

objs = \
	$(patsubst %.rkt,%.o,$(wildcard lib/*rkt)) \
	main.o \
	char.o \
	io.o \
	symbol.o \
	str.o \
	wrap.o \
	utf8.o \
	capi.o \
	unistring.o \
	sdl.o \
	math.o

default: runtime.o

main.o: villain.h runtime.h
wrap.o: villain.h types.h
char.o: villain.h utf8.h char.h
io.o: runtime.h villain.h utf8.h
symbol.o: str.h types.h villain.h
str.o: types.h villain.h
capi.o: runtime.h villain.h
unistring.o: villain.h
sdl.o: villain.h
math.o: villain.h

%.run:  %.o
	@ racket -t formdps.rkt -m make $@

%.run2: %.o runtime.o 
	gcc runtime.o $(shell cat modulefiles) $< -o $@ $(libs) # -lm
	@ racket -t formdps.rkt -m mv $@
	rm -f formdps $(shell cat modulefiles) modulefiles *.s

runtime.o: $(objs)
	ld -r $(objs) -o runtime.o

.SUFFIXES: .c .s .o .rkt
.c.o:
	gcc -fPIC -c -o $@ $<

.s.o:
	nasm -f $(format) -o $@ $<

lib/%.s: lib/%.rkt
	racket -t compile-library.rkt -m $< > $@

.rkt.s:
	racket -t compile-file.rkt -m $< > $@

clean:
	rm -f modulefiles formdps
	find .  -name "*.o" -delete -or \
		-name "*.s" -delete -or \
		-name "*.run" -delete
