UNAME := $(shell uname)
.PHONY: test std

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

%.run: %.o runtime.o
	gcc runtime.o $< -o $@ $(libs) -lm

runtime.o: main.o char.o io.o symbol.o str.o stdlibs
	ld -r main.o char.o io.o symbol.o str.o list.o math.o -o runtime.o
	# NOTE: append new stdlib object files to `-r` arguments above

stdlibs:
	make math.o
	make list.o
	# NOTE: add new stdlib make commands here

main.o: main.c types.h runtime.h
	gcc -fPIC -c main.c -o main.o

char.o: char.c types.h
	gcc -fPIC -c char.c -o char.o

io.o: io.c runtime.h
	gcc -fPIC -c io.c -o io.o

symbol.o: symbol.c str.h
	gcc -fPIC -c symbol.c -o symbol.o

str.o: str.c
	gcc -fPIC -c str.c -o str.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm -f *.o *.s
	find . -name *.run -delete
