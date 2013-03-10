(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (simpson-integral f a b n)
  (let ((h (/ (- b a) n)))
    (define (double-f x) (* 2 (f x)))
    (define (quad-f x) (* 4 (f x)))
    (define (double-next x) (+ x (* 2 h)))
    (* (/ h 3)
       (+ (f a) 
          (sum quad-f (+ a h) double-next b)
          (sum double-f (+ a (* 2 h)) double-next b)
          (f b)))))


(define (cube x) (* x x x))
(simpson-integral cube 0 1.0 100)
(simpson-integral cube 0 1.0 1000)
