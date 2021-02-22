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

runtime.o: main.o char.o io.o symbol.o str.o math.o list.o utf8.o
	ld -r main.o char.o io.o symbol.o str.o list.o math.o utf8.o -o runtime.o

main.o: main.c types.h runtime.h
	gcc -fPIC -c main.c -o main.o

char.o: char.c types.h utf8.h
	gcc -fPIC -c char.c -o char.o

io.o: io.c runtime.h utf8.h
	gcc -fPIC -c io.c -o io.o

symbol.o: symbol.c str.h
	gcc -fPIC -c symbol.c -o symbol.o

str.o: str.c
	gcc -fPIC -c str.c -o str.o

utf8.o: utf8.c
	gcc -fPIC -c utf8.c -o utf8.o

%.o: %.s
	nasm -f $(format) -o $@ $<

%.s: %.rkt
	racket -t compile-file.rkt -m $< > $@

clean:
	rm -f *.o *.s
	find . -name *.run -delete
