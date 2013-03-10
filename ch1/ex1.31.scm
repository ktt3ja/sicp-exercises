(define (prod-recur f a next b)
  (if (> a b)
    1
    (* (f a) (prod-recur f (next a) next b))))

(define (prod-iter f a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (f a) result))))
  (iter a 1))

(define (factorial-recur n)
  (prod-recur (lambda (n) n) 1 (lambda (x) (+ x 1)) n))

(define (factorial-iter n)
  (prod-iter (lambda (n) n) 1 (lambda (x) (+ x 1)) n))


;;; Test
(factorial-recur 0)  ; => 1
(factorial-recur 5)  ; => 120
(factorial-recur 10) ; => 3628800

(factorial-iter 0)   ; => 1
(factorial-iter 5)   ; => 120
(factorial-iter 10)  ; => 3628800


(define (pi-approx)
  (define (approx-half-pi n)
    (define (f x) (/ (* 4 (square x))
                     (- (* 4 (square x)) 1)))
    (prod-iter f 1.0 (lambda (x) (+ x 1)) n))
  (* 2 (approx-half-pi 10000)))

(pi-approx)
