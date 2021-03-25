#lang racket
(require "test-programs/get-progs.rkt")
(let () (begin (system "cp ./test-programs/villain/modules-example-*.rkt ..") (void)))
(for-each test-prog (get-progs "villain"))
(let () (begin (system "rm ../modules-example-*.*") (void)))

