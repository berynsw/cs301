#lang racket
; lab3.rkt
; Jessica Avery and Beryn Staub-Waldenberg
; Winter 2019
;
; List Filtering
; 2 main functions: superstitious-counting and find-primes.

; seq
;
; Produces a list of consecutive integers between two specified end-points
;
; Parameters:
;    i (integer): The smallest number that should appear in the list
;    j (integer): The largest number that should appear in the list
;    L (list) [OPTIONAL, default ’()]
;
; Returns:
;    A list consisting of the the consecutive integers from i to j
;    followed by L

(define (seq i j [L null])
  (if (> i j)
      L
      (seq i (- j 1) (cons j L))))


; superstitious-counting
;
; Produces a list of consecutive integers up to an endpoint and removes any numbers containing the provided digit
;
; Parameters:
;    digit (integer between 0 and 9): the digit we don't wanna see (the digit that should not appear in the list)
;    n (integer): The largest number that should appear in the list
;
; Returns:
;    A list consisting of the the consecutive integers from i to j
;    excluding the integers containing the digit we don't wanna see

(define (superstitious-counting digit n)
  (define (number-contains? x digit)
    (string-contains? (number->string x) (number->string digit)))
  (if (or (< digit 0) (> digit 9))
      null
      (filter (lambda (y) (not (number-contains? y digit))) (seq 1 n))))


; find-primes
;
; Produces a list of consecutive integers up to an endpoint and removes prime numbers
;
; Parameters: 
;    n (positive integer): The largest number that should appear in the list
;    start (integer): First prime number to filter out (defaults to 2)
;    list (list): List to be filtered (defaults to a sequence of consecutive integers from 2 to n)
;                 Starts at 2 because 1 isn't a prime number
;
; Returns:
;    A list consisting of the prime integers in the range 1 to n

(define (find-primes n [start 2] [list (seq 2 n)])
  (define (is-multiple? x y)
    (and (not (equal? x y)) (equal? (modulo x y) 0)))
  (if (< n 0)
      null
      (if (< start (sqrt n))
          (find-primes n (+ start 1) (filter (lambda (z) (not (is-multiple? z start))) list))
          list)))