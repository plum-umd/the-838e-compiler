#lang racket
(provide build-mgraph)
(require a86/printer "parse.rkt" "compile.rkt" "read.rkt" "ast.rkt"
         (submod a86/printer private))
(define mgraph '())       ; dependency graph of the modules in adjacency list format
(define mlist '())        ; list of the needed .o files of the required modules
(define mlist-fls '())   ; list of (cons f Lambda) for all function definitions
                          ; in the required modules
(define visited '())      ; list of visited files in building mgraph

(struct Mnode (file pvs fls e) #:prefab)

;; String(of filename) [Listof String] [Listof String] [Listof (Cons Id Lambda)]
;; Expr -> CMod (Struct)
(define (build-mgraph fn pvs rqs fls e)
  (build-mgraph-aux fn pvs rqs fls e '())
  (write-s-files mgraph fn)
  (write-obj-file-list mlist)
  (set! mlist-fls (remove-duplicates mlist-fls))
  ;(write-mgraph-to-file mgraph)
  (let ((rqs-Mn-lists (Mnode-adjacency-list-pairs rqs)))
    (let ((pv-exts (provided-functions-by-required-modules rqs-Mn-lists)))      
      (let ((fs (map car mlist-fls)) (ls (map cdr mlist-fls)))
        (CMod pv-exts pvs fs ls (map cdr fls) e)))))

;; String(of filename) [Listof String] [Listof String] [Listof (Cons Id Lambda)]
;; Expr [Listof String(of filenames)] -> Tail jump to build-next-layer function
;; Updates mlist and mgraph.
(define (build-mgraph-aux file pvs rqs fls e anscestors)
;  (with-output-to-file "debug"
;    #:exists 'append
;    (λ () (display "file: ") (displayln file) (display "rqs: ") (displayln rqs)
;         (display "anscestors: ") (displayln anscestors)
;         (display "pvs: ") (displayln pvs)))
        (set! mlist-fls (append fls mlist-fls))
        (set! mlist (append rqs mlist))
        (set! mgraph (remove-duplicates (cons (cons (Mnode file pvs fls e) rqs) mgraph)))
        (build-next-layer rqs (cons file anscestors)))


;; [Listof String(of filenames)] [Listof String(of filenames)] -> Void
;; Side effect: Forms mgraph (the dependency graph of modules and
;;              mlist (the list of modules that need to be compiled)
(define (build-next-layer rqs anscestors)
  (match rqs
    ['() (void)]
    [(cons rq rqs)
     (if (member rq anscestors)
         (error (format "There is a cycle in the dependency graph of modules"))

         (begin (if (member rq visited)
             (void)
             (begin
               (set! visited (cons rq visited))               
               (let ((p (open-input-file rq)))
                 (begin (read-line p)
                  (let ((s (parse (read p))))
                    (match s
                      [(Mod pvs new-rqs fls e)
                       (begin
                         (build-mgraph-aux rq pvs new-rqs fls e anscestors)
                         )]
                      [_ (error (format "the file ~a required is not a module" p))]))
                    (close-input-port p)))))
               (build-next-layer rqs anscestors)))]))

; [PairOf Mnode [ListOf Strings (of filenames)]] -> Void
;; Takes Mn-list which is an element of the mgraph list. Each element is a pair
;; of an Mnode struct and a list of strings of filenames representing the
;; adacency list of required files in the modules graph.
;; Side effect: traverses the mgraph list and compiles the module
;; file in each element of the list to the corresponding assembly file.
(define (write-s-files lst rootfile)
  (match lst
    ['() (void)]
    [(cons x xs)
     (begin (if (equal? (Mnode-file (car x)) rootfile) (void) (write-s-files-aux x))
            (write-s-files xs rootfile))]))

(define (write-s-files-aux Mn-list)
  (let ((rqs (cdr Mn-list)))
    (let ((rqs-Mn-lists (Mnode-adjacency-list-pairs rqs)))
      (let ((pv-exts (provided-functions-by-required-modules rqs-Mn-lists)))
        (let ((mnode (car Mn-list)))
          (let ((fs (map car (Mnode-fls mnode)))
                (ls (map cdr (Mnode-fls mnode))))
            (begin (with-output-to-file
                (path-replace-extension(Mnode-file (car Mn-list)) #".s")
              #:exists 'truncate
              (λ ()
                (parameterize ((current-shared? #t))
                 (displayln (asm-string (compile-module
                                          (CMod pv-exts (Mnode-pvs mnode)
                                                fs ls ls
                                               (Mnode-e mnode)) #f)))))))))))))

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
       (displayln (let ((lst (remove-duplicates 
            (map (λ (x) (string-append (string-trim x ".rkt") ".o"))
                           mlist))))
                    (if (zero? (length lst))
                        "n \n"
                        (string-append "y"
                                (foldr (λ (x acc) (string-append x " " acc)) ""
                         lst) "\n")))))))

;(define (write-mgraph-to-file mgraph)
;  (with-output-to-file "modulesgraph"
;     #:exists 'truncate
;     (λ ()
;       (displayln (foldr (λ (x acc) (string-append (format "~a \n" x) acc))
;                         "" mgraph)))))

    