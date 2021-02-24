#lang racket
(provide build-mgraph)
(require a86/printer "parse.rkt" "compile.rkt" "read.rkt" "ast.rkt"
         (submod a86/printer private))
(define mgraph '())
(define mlist '())

(struct Mnode (file pvs ds e) #:prefab)

;; String(of filename) [Listof String] [Listof String] [Listof Struct Defn(*)]
;; Expr -> CMod (Struct)
(define (build-mgraph fn pvs rqs ds e)
  (build-mgraph-aux fn pvs rqs ds e '())
  (match rqs
    ['() (void)]
    [(cons rq rqs)
     (let ((Mn-list (findf (λ (x) (equal? (Mnode-file (car x)) rq)) mgraph)))
       (write-s-files Mn-list))])
  (write-obj-file-list mlist)
  ;(write-mgraph-to-file mgraph)
  (let ((rqs-Mn-lists (Mnode-adjacency-list-pairs rqs)))
    (let ((pv-exts (provided-functions-by-required-modules rqs-Mn-lists)))
      (CMod pv-exts pvs ds e))))

;; String(of filename) [Listof String] [Listof String] [Listof Struct Defn(*)]
;; Expr [Listof String(of filenames)] -> Tail jump to build-next-layer function
;; Updates mlist and mgraph.
(define (build-mgraph-aux file pvs rqs ds e anscestors)
;  (with-output-to-file "debug"
;    #:exists 'append
;    (λ () (display "file: ") (displayln file) (display "rqs: ") (displayln rqs)
;         (display "anscestors: ") (displayln anscestors)
;         (display "pvs: ") (displayln pvs)))
  (set! mlist (append rqs mlist))
  (set! mgraph (cons (cons (Mnode file pvs ds e) rqs) mgraph))
  (build-next-layer rqs (cons file anscestors)))


;; [Listof String(of filenames)] [Listof String(of filenames)] -> Void
;; Side effect: Forms mgraph (the dependency graph of modules and
;;              mlist (the list of modules that need to be compiled)
(define (build-next-layer rqs anscestors)
  (match rqs
    ['() (void)]
    [(cons rq rqs)
;     (with-output-to-file "debug"
;       #:exists 'append
;       (λ ()
;         (display "build-next-layer rq: ") (displayln rq)
;         (display "anscestors: ") (displayln anscestors)))
     (if (member rq anscestors)
         (error (format "There is a cycle in the dependency graph of modules"))
         (let ((p (open-input-file rq)))
           (begin (read-line p)
                 ;(displayln (read p))
                  (match (parse (read p))
                    [(Mod pvs new-rqs ds e)
                     (begin
                       (build-mgraph-aux rq pvs new-rqs ds e anscestors)
                       (build-next-layer rqs anscestors))]
                    [_ (error (format "the file ~a required is not a module" p))])
                  (close-input-port p))))]))

; [PairOf Mnode [ListOf Strings (of filenames)]] -> Void
;; Takes Mn-list which is an element of the mgraph list. Each element is a pair
;; of an Mnode struct and a list of strings of filenames representing the
;; adacency list of required files in the modules graph.
;; Side effect: traverses the mgraph list by recursion and compiles the module
;; file in each element of the list to the corresponding assembly file.
(define (write-s-files Mn-list)
  (let ((rqs (cdr Mn-list)))
    (let ((rqs-Mn-lists (Mnode-adjacency-list-pairs rqs)))
      (let ((pv-exts (provided-functions-by-required-modules rqs-Mn-lists)))
        (begin 
          (with-output-to-file
              (path-replace-extension(Mnode-file (car Mn-list)) #".s")
            #:exists 'truncate
            (λ ()
              (parameterize ((current-shared? #t))
               (displayln (asm-string (compile-module
                                        (CMod pv-exts (Mnode-pvs (car Mn-list))
                                              (Mnode-ds (car Mn-list))
                                             (Mnode-e (car Mn-list))) #f))))))
          (for-each (λ (x) (write-s-files x)) rqs-Mn-lists))))))

;; [Listof String(of filenams)] -> [Listof [Pairof Mnode [Listof String(of filenames)]]
;; Auxilliary function that takes a list of module filenames and returns
;; the list of corresponding elements of mgraph
(define (Mnode-adjacency-list-pairs rqs)
  (foldr (λ (x acc)
           (cons (findf (λ (z) (equal? (Mnode-file (car z)) x)) mgraph) acc))
         '() rqs))


;; [Listof [Pairof Mnode [Listof String(of filenames)]] -> [Listof String(of filenames)]
;; Takes a list of elements of magraph and returns a list of all the functions
;; provided by the corresponding modules.
(define (provided-functions-by-required-modules rqs-Mn-lists)
  (remove-duplicates (foldr (λ (x acc) (append (Mnode-pvs (car x)) acc))
                                 '() rqs-Mn-lists)))    
      

(define (write-obj-file-list mlist)
  (with-output-to-file "modulefiles"
     #:exists 'truncate
     (λ ()
       (displayln (string-append (foldr (λ (x acc) (string-append x " " acc)) ""
                         (remove-duplicates 
            (map (λ (x) (string-append (string-trim x ".rkt") ".o"))
                           mlist))) "\n")))))

;(define (write-mgraph-to-file mgraph)
;  (with-output-to-file "modulesgraph"
;     #:exists 'truncate
;     (λ ()
;       (displayln (foldr (λ (x acc) (string-append (format "~a \n" x) acc))
;                         "" mgraph)))))


                          
                    

   
    