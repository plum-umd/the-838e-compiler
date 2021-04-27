#lang racket

(begin
  (struct empt () #:prefab)
  (empt? (empt)))
