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

  (define (draw x y)
    (let ([t (sdl/get-tick)])
      (+ (+ (* 3 x) (>> t 3))
            (band (<< 5 y)
                  (<< 3 x)))))
      #| (band (bxor (* 3 x) |#
      #|            (>> t 6)) |#
      #|       (bxor (* 3 y) |#
      #|            (>> t 7))))) |#
    #| (+ (bor (<< x 4) (<< y 4)) |# 
    #|    (>> (sdl/get-tick) 3))) |#

  (define (render fb coords w)
    (match coords
      ['() (void)]
      [(cons c cs)
       (let ([x (car c)]
             [y (cdr c)])
         (vector-set! fb (coord->idx x y w) (draw x y))
         (render fb cs w))]))

  (define (render-loop fb coords w)
    (sdl/poll-events)
    (render fb coords w)
    (sdl/render-fb fb)
    (render-loop fb coords w))

  (let ([width 256]
        [height 256]
        [scale 3])
    (let ([fb (make-vector (* width height) 0)]
          [coords (mk-coords width height)])

      (sdl/init width height scale)
      (render-loop fb coords width))))
