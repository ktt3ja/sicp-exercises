(define (f-recur n)
  (if (< n 3)
      n
      (+ (f-recur (- n 1))
         (* 2 (f-recur (- n 2)))
         (* 3 (f-recur (- n 3))))))

(f-recur 1)
(f-recur 3)
(f-recur 5)


(define (f n)
  (define (f-iter a b c count)
    (cond ((< count 0) count)
          ((= count 0) c)
          (else (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1)))))
  (f-iter 2 1 0 n))

(f 1)
(f 3)
(f 5)

