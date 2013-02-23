; require n >= 0 and 0 <= k <= n
(define (choose n k)
  (if (or (= k 0) (= k n))
      1
      (+ (choose (- n 1) (- k 1)) (choose (- n 1) k))))


(choose 3 2)
(choose 5 3)

