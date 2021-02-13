UNAME := $(shell uname)
.PHONY: test

ifeq ($(UNAME), Darwin)
  format=macho64
  libs='-lunistring'
else
  format=elf64
  libs='-l:libunistring.a'
endif

%.run: %.o runtime.o
	gcc runtime.o $< -o $@ $(libs) -lm

runtime.o: main.o char.o io.o
	ld -r main.o char.o io.o -o runtime.o

main.o: main.c types.h runtime.h
	gcc -fPIC -c main.c -o main.o

char.o: char.c types.h
	gcc -fPIC -c char.c -o char.o

io.o: io.c runtime.h
	gcc -fPIC -c io.c -o io.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm *.o *.s *.run
