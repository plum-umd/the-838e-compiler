#lang racket
(provide main)
(define (main arg1 arg2)
  (if (equal? arg1 "mv") 
      (let ((str_run (substring arg2 0 (- (string-length arg2) 1))))
        (begin (system (string-append "mv " arg2 " " str_run)) (void)))
      (begin (system "touch modulefiles")
             (let ((in (open-input-file "modulefiles")))
               (begin
                 (let ((str (read-line in)))
                   (if (eof-object? str)
                       (void)
                       (if (equal? (string-ref str 0) #\y)
                           (begin (string-set! str 0 #\ )
                                  (system (string-append "make " str)))
                           (void))))
                 (close-input-port in)))
             (let ((out (open-output-file "modulefiles" #:exists 'update)))
               (begin
                 (file-position out 0)
                 (display " " out)
                 (close-output-port out)))
             (begin (system (string-append arg1 " " arg2 "2")) (void)))))


