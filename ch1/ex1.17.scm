(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (* (+ a a) (/ b 2)))
        (else (+ a (* a (- b 1))))))
        

(* 3 4)
(* 4 5)
(* 0 4)
