(define random-seed 1)

(define (random)
  (set! random-seed (remainder (+ (* random-seed 32719) 3) 32749))
  (/ random-seed 32768.0)) 
  ; TODO use denominator > 32768

(define (jitter x)
  (* x (random)))
