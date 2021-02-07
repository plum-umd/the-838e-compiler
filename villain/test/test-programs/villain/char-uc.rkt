#lang racket
(cons (char-whitespace? #\a)
      (cons (char-whitespace? #\ )
            (cons (char-alphabetic? #\a)
                  (cons (char-alphabetic? #\ )
                        (cons (char-upcase #\a)
                              (cons (char-upcase #\A)
                                    (cons (char-downcase #\a)
                                          (cons (char-downcase #\A)
                                                (cons (char-titlecase #\a)
                                                      (cons (char-titlecase #\A)
                                                            '()))))))))))
                                          

