#lang racket

(begin
  (struct sprout (x y) #:prefab)

  (sprout-x (sprout 1 2)))
