#lang racket (require racket/flonum)
(if (fl<= (fl+ 7.5 17.2) (fl- 17.5 6.5)) (fl= 5.5 5.5) (fl+ 2.25 2.555555)) 