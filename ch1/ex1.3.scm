(define (sq2largest a b c)
        (define large1 (if (> a b) a b)) 
        (define small  (if (= large1 a) b a))
        (define large2 (if (> c small) c small))
        (+ (square large1) (square large2)))
