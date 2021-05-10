#lang racket

(begin
  (struct sprout (x y) #:prefab)

  (sprout? (sprout 1 2)))
