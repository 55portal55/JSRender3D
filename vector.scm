(define (zeroAdjust a)
  (if (= a 0.0)
    EPSILON
    a))

(define (normalize V)
  (let*
    ((x ((V 'get-x)))     
     (y ((V 'get-y)))     
     (z ((V 'get-z)))
     (mag (zeroAdjust (sqrt (+ (* x x) (* y y) (* z z))))))
    (make-point (/ x mag) (/ y mag) (/ z mag))))

(define (vec-length-1 V)
  (let*
    ((x ((V 'get-x)))     
     (y ((V 'get-y)))     
     (z ((V 'get-z))))
    (+ (* x x) (* y y) (* z z))))

(define (vec-length V)
  (let*
    ((x ((V 'get-x)))     
     (y ((V 'get-y)))     
     (z ((V 'get-z))))
    (sqrt (+ (* x x) (* y y) (* z z)))))

(define (cross-product X Y)
  (make-point
    (- (* ((X 'get-y)) ((Y 'get-z))) (* ((X 'get-z)) ((Y 'get-y))))
    (- (* ((X 'get-z)) ((Y 'get-x))) (* ((X 'get-x)) ((Y 'get-z))))
    (- (* ((X 'get-x)) ((Y 'get-y))) (* ((X 'get-y)) ((Y 'get-x))))))

(define (perpendicular-to-plane X Y)
  (normalize (cross-product X Y)))

(define v1 (make-point 1.0 1.0 1.0))

(define (v+ X Y)
  (make-point
    (+ ((X 'get-x)) ((Y 'get-x)))
    (+ ((X 'get-y)) ((Y 'get-y)))
    (+ ((X 'get-z)) ((Y 'get-z)))))

(define (v- X Y)
  (make-point
    (- ((X 'get-x)) ((Y 'get-x)))
    (- ((X 'get-y)) ((Y 'get-y)))
    (- ((X 'get-z)) ((Y 'get-z)))))

(define (v-scale X a)
  (make-point
    (* ((X 'get-x)) a)
    (* ((X 'get-y)) a)
    (* ((X 'get-z)) a)))

(define (v-clone X)
  (v-scale X 1.0))

(define (v+! X Y Z)
  ((Z 'set)
    (+ ((X 'get-x)) ((Y 'get-x)))
    (+ ((X 'get-y)) ((Y 'get-y)))
    (+ ((X 'get-z)) ((Y 'get-z)))))

(define (v-! X Y Z)
  ((Z 'set)
    (- ((X 'get-x)) ((Y 'get-x)))
    (- ((X 'get-y)) ((Y 'get-y)))
    (- ((X 'get-z)) ((Y 'get-z)))))

(define (v-scale! a X)
  ((X 'set)
    (* ((X 'get-x)) a)
    (* ((X 'get-y)) a)
    (* ((X 'get-z)) a)))

(define (dot X Y)
  (+
    (* ((X 'get-x)) ((Y 'get-x)))
    (* ((X 'get-y)) ((Y 'get-y)))
    (* ((X 'get-z)) ((Y 'get-z)))))

(define (v-color-scale c1 c2)
  (make-point
    (* ((c1 'get-x)) ((c2 'get-x)))
    (* ((c1 'get-y)) ((c2 'get-y)))
    (* ((c1 'get-z)) ((c2 'get-z)))))

(define (clip-upper V)
  (make-point
    (min ((V 'get-x)) 0.999999)
    (min ((V 'get-y)) 0.999999)
    (min ((V 'get-z)) 0.999999)))

(define (clip-lower V)
  (make-point
    (max ((V 'get-x)) 0.0)
    (max ((V 'get-y)) 0.0)
    (max ((V 'get-z)) 0.0)))
