(define (cube x) (* x x x))

(define (cuberoot x)
  (define (cuberoot-iter guess)
    (if (good-enough? guess)
        guess
        (cuberoot-iter (improve guess))))
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))
  (define (good-enough? guess)
    (< (abs (- (cube guess) x)) (* guess 1e-6)))
  (cuberoot-iter 1.0))

(cuberoot 27)
(cuberoot 64)
