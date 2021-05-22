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

  (define (draw x y t)
    (if (zero? (modulo (bxor (bxor x y) (>> t 10)) 11))
      255 0))

  (define (render fb coords w t)
    (match coords
      ['() (void)]
      [(cons c cs)
       (let ([x (car c)]
             [y (cdr c)])
         (vector-set! fb (coord->idx x y w) (draw x y t))
         (render fb cs w t))]))

  (define (render-loop fb coords w)
    (sdl/poll-events)
    (render fb coords w (sdl/get-tick))
    (sdl/render-fb fb)
    (render-loop fb coords w))

  (let ([width 256]
        [height 256]
        [scale 3])
    (let ([fb (make-vector (* width height) 0)]
          [coords (mk-coords width height)])

      (sdl/init width height scale)
      (render-loop fb coords width))))