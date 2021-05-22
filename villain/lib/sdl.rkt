#lang racket
(provide sdl/init
         sdl/poll-events
         sdl/get-tick
         sdl/render-fb)

(define (sdl/init width height scale)
  (ccall "sdl_init" width height scale))

(define (sdl/poll-events)
  (ccall "sdl_poll_events"))

(define (sdl/get-tick)
  (ccall "sdl_get_tick"))

(define (sdl/render-fb v)
  (ccall "sdl_render_fb" v))
