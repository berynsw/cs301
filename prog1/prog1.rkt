#lang racket

; prog1.rkt
; Beryn Staub-Waldenberg
; Winter 2019
;
; This program checks for relational properties on sets and builds composite relations

(define A (list 1 2 3))

; for-all
;
; Scans through a list and checks truth value based on an input lambda function
;
; Parameters:
;  list (list)
;  fx (function): inserted lambda function to check properties in list ex: does list contain the reverse of a given pair?
;
; Returns:
;  true or false
(define (for-all list fx)
  (cond
    [(equal? list null) #t]
    [(not (fx (car list))) #f]
    [else (for-all (cdr list) fx)]))


; contains?
;
; Scans through a list and checks if it contains an element, x
;
; Parameters:
;  x (element in list)
;  list (list)
;
; Returns:
;  true or false
(define (contains? x list)
  (cond
    [(equal? list null) #f]
    [(equal? (car list) x) #t]
    [else (contains? x (cdr list))]))
;(contains? '() A)


; reflexive?
;
; Scans through a list and checks if for all elements, x, in set A does relation R relate x to x
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (reflexive? A R)
  (for-all A (lambda (x)
              (if (contains? (cons x x) R)
                  #t
                  #f))))
;(define R (list (cons 0 1) (cons 0 0)))
;(reflexive? A R)


; irreflexive?
;
; Scans through a list and checks if for all elements, x, in set A does relation R not relate x to x
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (irreflexive? A R)
  (for-all A (lambda (x)
              (if (contains? (cons x x) R)
                  #f
                  #t))))
;(define R null)
;(irreflexive? A R)


; reverse-pair?
;
; Uses contains? function to check if a list contains the reverse of a given pair, x
; Helper function for symmetric? and anti-symmetric?
;
; Parameters:
;  list (list)
;  x (cons pair)
;
; Returns:
;  true or false
(define (reverse-pair? x list)
  (contains? (cons (cdr x) (car x)) list))


; symmetric?
;
; Scans through a list and checks if for all pairs (x y) in it it contains the reverse of the pair (y x)
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (symmetric? A R)
  (for-all R (lambda (x) (reverse-pair? x R))))
;(define R (list (cons 1 1) (cons 1 2) (cons 3 1)))
;(symmetric? A R)


; anti-symmetric?
;
; Scans through a list and checks if for all reverse pairs (x y) and (y x) x equals y
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (anti-symmetric? A R)
  (for-all R (lambda (x)
              (if (reverse-pair? x R)
                  (equal? (car x) (cdr x))
                  #t))))
;(define R (list (cons 1 1) (cons 3 2) (cons 2 3)))
;(anti-symmetric? A R)


; transitive?
;
; Scans through a list and checks if for any pair (x y) is there a pair that starts with y then is there a pair that starts with x and ends with what y ends with
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (transitive? A R)
  (for-all R (lambda (x)
              (for-all R (lambda (y)
                          (if (equal? (car y) (cdr x))
                              (contains? (cons (car x) (cdr y)) R)
                              #t))))))
;(define R (list (cons 3 4) (cons 2 1) (cons 1 1) (cons 1 2) (cons 2 2)))
;(transitive? A R)


; total?
;
; Scans through a set, A, and checks if for all elements in A is there a relation between every element (could be x Relates to y or y Relates to x) including themselves
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (total? A R)
  (for-all A (lambda (x)
              (for-all A (lambda (y)
                          (or (contains? (cons x y) R) (contains? (cons y x) R)))))))
;(define R (list (cons 1 2) (cons 2 3) (cons 3 1) (cons 1 1) (cons 2 2) (cons 3 3)))
;(total? A R) 


; equivalence?
;
; Checks if a relation, R, is reflexive, symmetric, and transative
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (equivalence? A R)
  (and (and (reflexive? A R) (symmetric? A R)) (transitive? A R)))
;(define R (list (cons 1 1) (cons 2 2) (cons 3 3)))
;(equivalence? A R)


; poset?
;
; Checks if a relation, R, is reflexive, anti-symmetric, and transative
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (poset? A R)
  (and (and (reflexive? A R) (anti-symmetric? A R)) (transitive? A R)))


; totally-ordered?
;
; Checks if a relation, R, is total, anti-symmetric, and transative
;
; Parameters:
;  A (list): a set
;  R (list): a relation on A
;
; Returns:
;  true or false
(define (totally-ordered? A R)
  (and (and (total? A R) (anti-symmetric? A R)) (transitive? A R)))


; compose
;
; Scans through the first relation list, R1, and passes its first element pair and the second relation list, R2, to compose-helper
;
; Parameters:
;  A (list): a set 
;  R1 (list): the first relation set to be used in composition
;  R2 (list): the second relation set
;  R2-copy (list): a copy of the second relation set which is used to reset R2 to original in outer calls [OPTIONAL, default R2]
;  result (list): a composed relational set [OPTIONAL, default null]
;
; Returns:
;  A resulting set of pairs
(define (compose A R1 R2 [R2-copy R2] [result null])
  (cond
    [(equal? R1 null) result] 
    [(cond
       [(equal? R2 null) (compose A (cdr R1) R2-copy R2-copy result)] 
       [(equal? (cdr (car R1)) (car (car R2)))   (compose A R1 (cdr R2) R2-copy (cons (cons (car (car R1)) (cdr (car R2))) result))]
       [else (compose A R1 (cdr R2) R2-copy result)])] 
    [else (compose A (cdr R1) R2-copy R2-copy result)]))

;(define R1 (list  (cons 1 2) (cons 3 2) (cons 7 2)))
;(define R2 (list  (cons 2 1) (cons 2 3)))
;(compose A R1 R2)


; reflexive-closure
;
; Scans through the set, A, and checks if the relation contains the pair (x x), if it doesnt (x x) is added to the resulting list
;
; Parameters:
;  A (list): a set 
;  R (list): a relation on set A
;  result (list): the original relation set, R, with minimal pairs added to make it reflexive [OPTIONAL, default R]
;
; Returns:
;  A resulting set of pairs
(define (reflexive-closure A R [result R])
  (cond
    [(equal? A null) result]
    [(not (contains? (cons (car A) (car A)) R))
          (reflexive-closure (cdr A) R (cons (cons (car A) (car A)) result))]
    [else (reflexive-closure (cdr A) R result)]))
;(define R (list (cons 1 2) (cons 3 4)))
;(reflexive-closure A R)


; symmetric-closure
;
; Scans through the relation set, R, and checks if the relation contains a reverse pair for each of its pairs. If it doesn't the reverse pair is added to the resulting list of pairs.
;
; Parameters:
;  A (list): a set 
;  R (list): a relation on set A
;  result (list): the original relation set, R, with minimal pairs added to make it symmetric [OPTIONAL, default R]
;
; Returns:
;  A resulting set of pairs
(define (symmetric-closure A R [result R])
  (cond
    [(equal? R null) result]
    [(not (reverse-pair? (cons (car (car R)) (cdr (car R))) result))
          (symmetric-closure A (cdr R) (cons (cons (cdr (car R)) (car (car R))) result))]
    [else (symmetric-closure A (cdr R) result)]))
;(define R (list (cons 1 2) (cons 3 4) (cons 1 1) (cons 5 6)))
;(symmetric-closure A R)


; transitive-closure
;
; Takes in a relation set, R, and adds minimal pairs to make it transitive
; Uses compose function to compose R with R and adds that to the original list
;
; Parameters:
;  A (list): a set 
;  R (list): a relation on set A
;  R-inner (list): a copy of R to be used in the inner, nested recursive call [OPTIONAL, default R]
;  R-copy (list): static copy of R that's used to reset R-inner in outer calls [OPTIONAL, default R]
;  result (list): the original relation set, R, with minimal pairs added to make it transitive [OPTIONAL, default R]
;
; Returns:
;  A resulting set of pairs
(define (transitive-closure A R [R-inner R] [R-copy R] [result R])
  (cond
    [(equal? R null) result] 
    [(cond
       [(equal? R-inner null) (transitive-closure A (cdr R) R-copy R-copy result)] 
       [(and (equal? (cdr (car R)) (car (car R-inner))) (not (transitive? A result))) (transitive-closure A R (cdr R-inner) R-copy (cons (cons (car (car R)) (cdr (car R-inner))) result))]
       [else (transitive-closure A R (cdr R-inner) R-copy result)])] 
    [else (transitive-closure A (cdr R) R-copy R-copy result)]))
;(define R (list (cons 3 2)  (cons 2 3) (cons 1 2) (cons 2 1) ))
;(transitive-closure A R)
          
