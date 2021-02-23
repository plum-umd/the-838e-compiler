UNAME := $(shell uname)
.PHONY: test std default

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

objs = \
	$(patsubst %.rkt,%.o,$(wildcard lib/*rkt)) \
	main.o \
	char.o \
	io.o \
	symbol.o \
	str.o \
	wrap.o

default: runtime.o

main.o: villain.h runtime.h
char.o: villain.h
io.o: runtime.h villain.h
symbol.o: str.h
wrap.o: villain.h types.h

%.run: %.o runtime.o
	gcc runtime.o $< -o $@ $(libs)

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
	find .  -name "*.o" -delete -or \
		-name "*.s" -delete -or \
		-name "*.run" -delete
