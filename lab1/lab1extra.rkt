#lang slideshow

(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
  (filled-rectangle n n))

(define (four p)
  (define two-p (hc-append p p))
  (vc-append two-p two-p))

(define (checker p1 p2)
  (let ([p12 (hc-append p1 p2)]
        [p21 (hc-append p2 p1)])
    (vc-append p12 p21)))
(checker (colorize (square 10) "red")
           (colorize (square 10) "black"))

(define (checkerboard p)
  (let* ([rp (colorize p "red")]
         [bp (colorize p "black")]
         [c (checker rp bp)]
         [c4 (four c)])
    (four c4)))
(checkerboard (square 10))

(define (series mk)
  (hc-append 4 (mk 5) (mk 10) (mk 20)))
(series circle)
(series (lambda (size) (checkerboard (square size))))

(define (rgb-series mk)
  (vc-append
   (series (lambda (sz) (colorize (mk sz) "red")))
   (series (lambda (sz) (colorize (mk sz) "green")))
   (series (lambda (sz) (colorize (mk sz) "blue")))))
(rgb-series square)

(define (rgb-maker mk)
  (lambda (sz)
    (vc-append (colorize (mk sz) "red")
               (colorize (mk sz) "green")
               (colorize (mk sz) "blue"))))
(series (rgb-maker circle))

(define (rainbow p)
  (map (lambda (color)
         (colorize p color))
       (list "red" "orange" "yellow" "green" "blue" "purple")))
(rainbow (square 2))
(apply vc-append (rainbow (square 20)))

(define (pyramid x)
  (vc-append x (hc-append x x)))
(pyramid (circle 20))

(define (nested-pyramids shape nestNum)
  (cond
    [(< nestNum 2) (pyramid shape)]
        [(pyramid (nested-pyramids shape (- nestNum 1)))]))

(nested-pyramids (circle 6) 3)


(define (triNester shape nestNum [counter 1] [product (pyramid (shape))])
  (if (> counter nestNum)
      product
      (triNester shape nestNum (+ counter 1) (pyramid (product)))))

(triNester (circle 10) 4)
