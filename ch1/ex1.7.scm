; The problem with the original good-enough implementation is that the bound
; check is not precise enough for small number and is too precise for
; large number (precision of number is limited in computer).
; As a result, with the current good-enough implementation, sqrt x is not
; accurate when x is small and never terminates when x is large.
; 
; This new good-enough implementation check for bound as a fraction of
; guess so that the bound reflects the necessary precision based on
; the magnitude of sqrt(x).

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (average x y)
    (/ (+ x y) 2))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) (* guess 1e-6)))
  (sqrt-iter 1.0))


(sqrt 2)
(sqrt 0.000000000001)
(sqrt 34252787963450222608)
