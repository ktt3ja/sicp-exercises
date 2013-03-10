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


; primes
(prime? 2)
(prime? 5)
(prime? 101)
(prime? 3571)
(prime? 27644437) 

; Carmichael number
(prime? 1105)
(prime? 1729)
(prime? 2465)
(prime? 2821)
(prime? 6601)
(prime? 8911)

; other non-prime
(prime? 3569) 
(prime? 252624629)
(prime? 4325263636260)
