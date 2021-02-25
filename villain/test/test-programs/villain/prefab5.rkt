#lang racket

(begin
  (struct sprout (x y) #:prefab)

  (sprout-y (sprout 1 2)))
