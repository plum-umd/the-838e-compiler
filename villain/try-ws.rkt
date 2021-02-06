#lang racket
(system "make runtime.o")
(system "make ws.o")
(system "gcc -z defs -v -lunistring -shared runtime.o ws.o -o ws.so")
