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
      (+ (bor (<< x 2) (<< y 2))
         (>> t 7))))

  (define (render fb coords w)
    (match coords
      ['() (void)]
      [(cons c cs)
       (let ([x (car c)]
             [y (cdr c)])
         (vector-set! fb (coord->idx x y w) (draw x y))
         (render fb cs w))]))

  (define (render-loop fb coords w)
    (sdl/pull-events)
    (render fb coords w)
    (sdl/render-fb fb)
    (render-loop fb coords w))

  (let ([width 256.0]
        [height 256.0]
        [scale 3.0])
    (let ([w (exact-truncate width)]
          [h (exact-truncate height)])
      (let ([fb (make-vector (* w h) 0)]
            [coords (mk-coords w h)])

        (sdl/init width height scale)
        (render-loop fb coords w)))))
