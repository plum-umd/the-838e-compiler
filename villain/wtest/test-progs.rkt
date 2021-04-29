#lang racket
(require "test-programs/get-progs.rkt")
(for-each test-prog (get-progs "fraud"))

