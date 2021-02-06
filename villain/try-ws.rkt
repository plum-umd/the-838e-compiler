#lang racket
(system "make try.o")
(system "make runtime.o")
(system "gcc -z defs -v -lunistring -shared try.o runtime.o -o try.so")

