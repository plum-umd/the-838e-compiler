#lang racket
(begin
  (define (mk-coords* x y w h acc)
    (if (and (= x 0)
             (= y 0))
      (cons (cons x y) acc)
      (if (= x 0)
        (mk-coords* (- w 1) (- y 1) w h
                    (cons (cons x y) acc))
        (mk-coords* (- x 1) y w h
                    (cons (cons x y) acc)))))
  (define (mk-coords w h)
    (mk-coords* (- w 1) (- h 1) w h '()))

  (define (coord->idx x y w)
    (+ (* y w) x))

  (define (render fb coords w)
    (match coords
      ['() (void)]
      [(cons c cs)
       (let ([x (car c)]
             [y (cdr c)])
          (if (= (modulo (* (sdl/get-tick)
                           (+ x y))
                         2) 
                 0)
            (vector-set! fb (coord->idx (car c) (cdr c) w) 255)
            (vector-set! fb (coord->idx (car c) (cdr c) w) 0))
          (render fb cs w))]))

  (define (render-loop fb coords w)
    (sdl/pull-events)
    (render fb coords w)
    (sdl/render-fb fb)
    (render-loop fb coords w))

  (let ([width 32.0]
        [height 32.0]
        [scale 8.0])
    (let ([w (exact-truncate width)]
          [h (exact-truncate height)])
      (let ([fb (make-vector (* w h) 0)]
            [coords (mk-coords w h)])

        (sdl/init width height scale)
        (render-loop fb coords w)))))
