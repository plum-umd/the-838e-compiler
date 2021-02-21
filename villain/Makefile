UNAME := $(shell uname)
.PHONY: test std default

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

objs_lib = \
	bool.o \
	math.o \
	list.o

objs = \
	$(objs_lib) \
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
	gcc formdps.c -o formdps
	./formdps make $@

%.run2: %.o runtime.o 
	gcc runtime.o $< -o $@ $(libs) -lm
	./formdps mv $@
	rm -f modulefiles formdps

runtime.o: $(objs) $(shell cat modulefiles)
	ld -r $(shell cat modulefiles) $(objs) -o runtime.o

.SUFFIXES: .c .s .o .rkt
.c.o:
	gcc -fPIC -c -o $@ $< 

.s.o:
	nasm -f $(format) -o $@ $<

.rkt.s:
	racket -t compile-file.rkt -m $< > $@

clean:
	rm -f *.o *.s modulefiles formdps
	find . -name "*.run" -delete
