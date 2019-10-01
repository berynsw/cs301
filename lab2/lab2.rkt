#lang racket
(define (permutations size)
(let ((elements (list #t #f)))
(if (zero? size)
'(())
(append-map (lambda (p)
(map (lambda (e) (cons e p)) elements))
(permutations (sub1 size))))))

#|(define (truth-table wff)
  (define (curPerm (permutations (procedure-arity wff))))
  (define (evaluate list)
  (apply wff list))
  (map evaluate curPerm))

(truth-table wff1)
|#


(define (implies P Q)
  (or (not P) Q))

; P -> (Q -> R)
(define (wff1 P Q R)
  (implies P (implies Q R)))

; Q -> (P -> R)
(define (wff2 P Q R)
  (implies Q (implies P R)))

; R -> (Q -> P)
(define (wff3 P Q R)
  (implies R (implies Q P)))


; wff4 and 5 are equivalent
; P v Q
(define (wff4 P Q)
  (or P Q))

; (P v Q) v (P v Q)
(define (wff5 P Q)
  (or (or P Q) (or P Q)))


; P -> (Q ^ R)
(define (wff6 P Q R)
  (implies P (and Q R)))


; P ^ (Q v R)
(define (wff7 P Q R)
  (and P (or Q R)))

; (((P ^ Q) ^ R) ^ S)
(define (wff8 P Q R S)
  (or S (or R (or P Q))))

; (((P v Q) v R) v S)
(define (wff9 P Q R S)
  (and S (and R (and P Q))))

(define (truth-table wff)
  #|(define (evaluate list)
    (apply wff list))|#

  (map (lambda (list) (apply wff list)) (permutations (procedure-arity wff))))

(define (equivalent? wffA wffB)
  (equal? (truth-table wffA) (truth-table wffB)))
