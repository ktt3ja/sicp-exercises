(define (filtered-accumulate valid? combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((valid? a)
         (combiner (term a)
                   (filtered-accumulate valid?
                                        combiner
                                        null-value
                                        term
                                        (next a)
                                        next b)))
        (else (filtered-accumulate valid?
                                   combiner
                                   null-value
                                   term
                                   (next a)
                                   next
                                   b))))

;;;;; Part b

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (coprime-prod n)
  (define (coprime? a)
    (= (gcd a n) 1))
  (filtered-accumulate coprime? * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(coprime-prod 10) ; => 189
(coprime-prod 15) ; => 896896


;;;;; Part a
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((x (expmod base (/ exp 2) m))
                (x_sq_mod_m (remainder (square x) m)))
           (if (and (not (= x 1))
                    (not (= x (- m 1)))
                    (= x_sq_mod_m 1))
               0
               x_sq_mod_m)))
        (else (remainder (* base
                            (expmod base (- exp 1) m))
                         m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (prime? n)
  (define (try times)
    (cond ((= times 0) true)
          ((miller-rabin-test n) (try (- times 1)))
          (else false)))
  (if (= n 1)
    #f
    (try 40)))

(define (prime-sum a b)
  (filtered-accumulate prime? + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))

(prime-sum 1 3) ; => 5
(prime-sum 5 15) ; => 36 

