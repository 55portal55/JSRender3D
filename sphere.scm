(define (make-sphere center radius shader)
  (set! scene-objects (cons
    (make-sphere-proper center radius shader)
    scene-objects)))

(define (make-sphere-proper center radius shader)
  (lambda (selector)
    (case selector
      ((position)
       (lambda ()
         center))
      ((radius)
       (lambda ()
         radius))
      ((set-radius)
       (lambda (r)
         (set! radius r)))
      ((get-shader)
       (lambda ()
         shader))
      ((get-bounding-box)
       (lambda ()
         (make-bounding-box
           (make-point
             (- ((center 'get-x)) radius)
             (- ((center 'get-y)) radius)
             (- ((center 'get-z)) radius))
           (make-point
             (+ ((center 'get-x)) radius)
             (+ ((center 'get-y)) radius)
             (+ ((center 'get-z)) radius)))))
      ((get-normal)
       (lambda (intersection-point)
         (let*
           ((x1 ((intersection-point 'get-x)))
            (y1 ((intersection-point 'get-y)))
            (z1 ((intersection-point 'get-z)))
            (x2 ((center 'get-x)))
            (y2 ((center 'get-y)))
            (z2 ((center 'get-z)))
            (delta-x (- x2 x1))
            (delta-y (- y2 y1))
            (delta-z (- z2 z1))
            (x/radius (/ delta-x radius))
            (y/radius (/ delta-y radius))
            (z/radius (/ delta-z radius)))
           (make-point x/radius y/radius z/radius))))
      ((display)
       (lambda ()
         (display "center: ") ((center 'display))
         (display " radius: ") (display radius)))
      ((intersect-ray)
       (lambda (ray-source dir-x dir-y dir-z shadow-test)
         (let*
           ((x1 ((ray-source 'get-x)))
            (y1 ((ray-source 'get-y)))
            (z1 ((ray-source 'get-z)))
            (x2 (+ x1 dir-x))
            (y2 (+ y1 dir-y))
            (z2 (+ z1 dir-z))
            (x3 ((center 'get-x)))
            (y3 ((center 'get-y)))
            (z3 ((center 'get-z)))
            (x2-x1 (- x2 x1))
            (y2-y1 (- y2 y1))
            (z2-z1 (- z2 z1))
            (a (+ (* x2-x1 x2-x1) (* y2-y1 y2-y1) (* z2-z1 z2-z1)))
            (b (* 2.0 (+ (* x2-x1 (- x1 x3))
                       (* y2-y1 (- y1 y3))
                       (* z2-z1 (- z1 z3)))))
            (c (- (- (+ (* x3 x3)
                        (* y3 y3)
                        (* z3 z3)
                        (* x1 x1)
                        (* y1 y1)
                        (* z1 z1))
                     (* 2.0 (+ (* x3 x1)
                             (* y3 y1)
                             (* z3 z1))))
                     (* radius radius)))
            (i (- (* b b) (* 4.0 a c))))
           (cond
             ((negative? i) ; no intersection
               '())
             ((zero? i) ; one point
               (let
                 ((mu (- (/ b (* 2.0 a)))))
                 (make-point
                   (+ x1 (* mu x2-x1))
                   (+ y1 (* mu y2-y1))
                   (+ z1 (* mu z2-z1)))))
             (else ; two points 
               (let*
                 ((radical (sqrt i))
                  (denominator (* 2.0 a))
                  (mu1 (/ (+ (- b) radical) denominator))
                  (mu2 (/ (- (- b) radical) denominator))
                  (intersection-point-1
                    (make-point
                      (+ x1 (* mu1 x2-x1))
                      (+ y1 (* mu1 y2-y1))
                      (+ z1 (* mu1 z2-z1))))
                  (intersection-point-2
                    (make-point
                      (+ x1 (* mu2 x2-x1))
                      (+ y1 (* mu2 y2-y1))
                      (+ z1 (* mu2 z2-z1)))))
                 (if shadow-test
                   (if (> ((ray-source 'distance) intersection-point-1)
                          ((ray-source 'distance) intersection-point-2))
                     intersection-point-1
                     intersection-point-2)
                   (if (< ((ray-source 'distance) intersection-point-1)
                          ((ray-source 'distance) intersection-point-2))
                     intersection-point-1
                     intersection-point-2))))))))
      ((intersect-ray-fisheye)
       (lambda (ray-source dir-x dir-y dir-z fisheye-up)
         (let*
           ((x1 ((ray-source 'get-x)))
            (y1 ((ray-source 'get-y)))
            (z1 ((ray-source 'get-z)))
            (x2 (+ x1 dir-x))
            (y2 (+ y1 dir-y))
            (z2 (+ z1 dir-z))
            (x3 ((center 'get-x)))
            (y3 ((center 'get-y)))
            (z3 ((center 'get-z)))
            (x2-x1 (- x2 x1))
            (y2-y1 (- y2 y1))
            (z2-z1 (- z2 z1))
            (a (+ (* x2-x1 x2-x1) (* y2-y1 y2-y1) (* z2-z1 z2-z1)))
            (b (* 2.0 (+ (* x2-x1 (- x1 x3))
                       (* y2-y1 (- y1 y3))
                       (* z2-z1 (- z1 z3)))))
            (c (- (- (+ (* x3 x3)
                        (* y3 y3)
                        (* z3 z3)
                        (* x1 x1)
                        (* y1 y1)
                        (* z1 z1))
                     (* 2.0 (+ (* x3 x1)
                             (* y3 y1)
                             (* z3 z1))))
                     (* radius radius)))
            (i (- (* b b) (* 4.0 a c))))
           (cond
             ((negative? i) ; no intersection
               '())
             ((zero? i) ; one point
               (let
                 ((mu (- (/ b (* 2.0 a)))))
                 (make-point
                   (+ x1 (* mu x2-x1))
                   (+ y1 (* mu y2-y1))
                   (+ z1 (* mu z2-z1)))))
             (else ; two points 
               (let*
                 ((radical (sqrt i))
                  (denominator (* 2.0 a))
                  (mu1 (/ (+ (- b) radical) denominator))
                  (mu2 (/ (- (- b) radical) denominator))
                  (intersection-point-1
                    (make-point
                      (+ x1 (* mu1 x2-x1))
                      (+ y1 (* mu1 y2-y1))
                      (+ z1 (* mu1 z2-z1))))
                  (intersection-point-2
                    (make-point
                      (+ x1 (* mu2 x2-x1))
                      (+ y1 (* mu2 y2-y1))
                      (+ z1 (* mu2 z2-z1)))))
                 (if (< fisheye-up 0.0)
                   (begin
                     (if (> ((ray-source 'distance) intersection-point-1)
                            ((ray-source 'distance) intersection-point-2))
                       intersection-point-1
                       intersection-point-2))
                   (begin
                     (if (< ((ray-source 'distance) intersection-point-1)
                            ((ray-source 'distance) intersection-point-2))
                       intersection-point-1
                       intersection-point-2)))))))))
      (else
       (render-error "make-sphere" "Invalid selector: " selector)))))
