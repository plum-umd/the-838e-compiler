#lang racket
;(system "make try.o")
(require "compile-file.rkt")
(require (submod a86/printer private))
(parameterize ((current-shared? #t))
  (with-output-to-file "try.o"
    (lambda ()  (main "try.rkt"))
    #:exists 'replace))

(system "make runtime.o")
(system "gcc -z defs -v -lunistring -shared try.o runtime.o -o try.so")

