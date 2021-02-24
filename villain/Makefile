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
	str.o

default: runtime.o

main.o: types.h runtime.h
char.o: types.h
io.o: runtime.h
symbol.o: str.h

%.run:  %.o
	@ gcc formdps.c -o formdps
	@./formdps make $@

%.run2: %.o runtime.o 
	gcc runtime.o $(shell cat modulefiles) $< -o $@ $(libs) -lm
	@./formdps mv $@
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
