(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0)
           a)
          ((even? n)
           (iter a (square b) (/ n 2)))
          (else
           (iter (* a b) b (- n 1)))))
  (iter 1 b n))


(fast-expt 2 3)
(fast-expt 2 7)
(fast-expt 2 4)
(fast-expt 3 5)
