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

cl:
	find .  -name "*.o" -not -name \
		"runtime.o" -not -path \
	        "./lib/*" -delete -or \
		-name "*.s" -delete -or \
		-name "*.run" -delete



