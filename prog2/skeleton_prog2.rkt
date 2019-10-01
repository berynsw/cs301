#lang racket

;; Useful helper functions

(define (contains? x S)
  (cond ((null? S) false)
        ((equal? x (car S)) true)
        (else (contains? x (cdr S)))))

(define (implies P Q)
  (or (not P) Q))

;; Universe definitions

(define A '(hutchinson hearne fizzano 241 301 474 493 dennis doris spruce redwood rose))
(define altA (list 1 2 3))

;; Relation definitions

(define is-faculty '((hutchinson) (hearne) (fizzano)))

(define is-course '((301) (241) (474) (493)))

(define is-student '((dennis) (doris)))

; (a,b,c) means faculty member a teaches course b to student c
(define teaches-to '((hutchinson 301 dennis) (hutchinson 241 doris) (hearne 301 doris) (hearne 493 dennis) (fizzano 474 doris))) 

(define is-person '((hutchinson) (hearne) (fizzano) (dennis) (doris)))

(define is-tree '((spruce) (redwood)))

(define is-plant '((spruce) (redwood) (rose)))

(define taller-than '((redwood spruce) (redwood doris) (redwood dennis) (spruce doris) (spruce dennis) (dennis doris)))

; altR matches altA (useful for testing binary relations)
(define altR (list (cons 1 2) (cons 1 3) (cons 2 2) (cons 3 3) (cons 3 1) (cons 1 1) (cons 2 3)))

;; Predicate definitions

(define (is-faculty? x)
  (contains? (list x) is-faculty))

(define (is-course? x)
  (contains? (list x) is-course))

(define (is-student? x)
  (contains? (list x) is-student))

(define (teaches-to? prof course student)
  (contains? (list prof course student) teaches-to))

(define (is-person? x)
	(contains? (list x) is-person))

(define (is-tree? x)
  (contains? (list x) is-tree))

(define (is-plant? x)
  (contains? (list x) is-plant))

(define (taller-than? x y)
	(contains? (list x y) taller-than))
